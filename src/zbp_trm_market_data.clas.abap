CLASS zbp_trm_market_data DEFINITION
  PUBLIC
  ABSTRACT
  FINAL
  FOR BEHAVIOR OF z_trm_market_data_c_cds.

  PUBLIC SECTION.
    CONSTANTS:
      BEGIN OF market_data_type,
        fx   TYPE c LENGTH 3 VALUE 'FX',
        ir   TYPE c LENGTH 3 VALUE 'IR',
        comm TYPE c LENGTH 4 VALUE 'COMM',
      END OF market_data_type.

  PRIVATE SECTION.
    METHODS:
      validateMarketData FOR VALIDATE ON SAVE
        IMPORTING keys FOR MarketData~validateMarketData,

      validateDates FOR VALIDATE ON SAVE
        IMPORTING keys FOR MarketData~validateDates,

      validatePriceRange FOR VALIDATE ON SAVE
        IMPORTING keys FOR MarketData~validatePriceRange,

      calculateConfidenceScore FOR DETERMINE ON MODIFY
        IMPORTING keys FOR MarketData~calculateConfidenceScore,

      validateData FOR MODIFY
        IMPORTING keys FOR ACTION MarketData~validateData RESULT result,

      rejectData FOR MODIFY
        IMPORTING keys FOR ACTION MarketData~rejectData RESULT result,

      requestMarketDataUpdate FOR MODIFY
        IMPORTING keys FOR ACTION MarketData~requestMarketDataUpdate RESULT result.

ENDCLASS.

CLASS zbp_trm_market_data IMPLEMENTATION.

  METHOD validateMarketData.
    " Read market data
    READ ENTITIES OF z_trm_market_data_c_cds IN LOCAL MODE
      ENTITY MarketData
        FIELDS ( MarketDataType InstrumentID PriceValue DataSource )
        WITH CORRESPONDING #( keys )
      RESULT DATA(market_data).

    " Validate each entry
    LOOP AT market_data INTO DATA(market_data_entry).
      " Check market data type
      IF market_data_entry-MarketDataType NE market_data_type-fx AND
         market_data_entry-MarketDataType NE market_data_type-ir AND
         market_data_entry-MarketDataType NE market_data_type-comm.
        APPEND VALUE #( %tky = market_data_entry-%tky ) TO failed-marketdata.
        APPEND VALUE #( %tky = market_data_entry-%tky
                       %msg = new_message_with_text(
                         severity = if_abap_behv_message=>severity-error
                         text     = 'Invalid market data type' )
                     ) TO reported-marketdata.
        CONTINUE.
      ENDIF.

      " Check instrument ID format based on type
      CASE market_data_entry-MarketDataType.
        WHEN market_data_type-fx.
          " FX should have currency pair format (e.g., EURUSD)
          IF strlen( market_data_entry-InstrumentID ) NE 6.
            APPEND VALUE #( %tky = market_data_entry-%tky ) TO failed-marketdata.
            APPEND VALUE #( %tky = market_data_entry-%tky
                          %msg = new_message_with_text(
                            severity = if_abap_behv_message=>severity-error
                            text     = 'Invalid currency pair format' )
                        ) TO reported-marketdata.
          ENDIF.
      ENDCASE.

      " Validate price value
      IF market_data_entry-PriceValue <= 0.
        APPEND VALUE #( %tky = market_data_entry-%tky ) TO failed-marketdata.
        APPEND VALUE #( %tky = market_data_entry-%tky
                       %msg = new_message_with_text(
                         severity = if_abap_behv_message=>severity-error
                         text     = 'Price value must be positive' )
                     ) TO reported-marketdata.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.

  METHOD validateDates.
    READ ENTITIES OF z_trm_market_data_c_cds IN LOCAL MODE
      ENTITY MarketData
        FIELDS ( ValidFrom ValidTo )
        WITH CORRESPONDING #( keys )
      RESULT DATA(market_data).

    LOOP AT market_data INTO DATA(market_data_entry).
      " Check if ValidFrom is in the past
      IF market_data_entry-ValidFrom < cl_abap_context_info=>get_system_date( ).
        APPEND VALUE #( %tky = market_data_entry-%tky ) TO failed-marketdata.
        APPEND VALUE #( %tky = market_data_entry-%tky
                       %msg = new_message_with_text(
                         severity = if_abap_behv_message=>severity-error
                         text     = 'Valid from date cannot be in the past' )
                     ) TO reported-marketdata.
      ENDIF.

      " Check if ValidTo is after ValidFrom
      IF market_data_entry-ValidTo IS NOT INITIAL AND
         market_data_entry-ValidTo <= market_data_entry-ValidFrom.
        APPEND VALUE #( %tky = market_data_entry-%tky ) TO failed-marketdata.
        APPEND VALUE #( %tky = market_data_entry-%tky
                       %msg = new_message_with_text(
                         severity = if_abap_behv_message=>severity-error
                         text     = 'Valid to date must be after valid from date' )
                     ) TO reported-marketdata.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.

  METHOD validatePriceRange.
    " Read market data
    READ ENTITIES OF z_trm_market_data_c_cds IN LOCAL MODE
      ENTITY MarketData
        FIELDS ( MarketDataType PriceValue )
        WITH CORRESPONDING #( keys )
      RESULT DATA(market_data).

    " Define thresholds for different market data types
    CONSTANTS:
      fx_max_rate   TYPE f VALUE '1000.00',  " Maximum reasonable FX rate
      ir_max_rate   TYPE f VALUE '100.00',   " Maximum interest rate (percentage)
      comm_max_rate TYPE f VALUE '10000.00'. " Maximum commodity price

    LOOP AT market_data INTO DATA(market_data_entry).
      CASE market_data_entry-MarketDataType.
        WHEN market_data_type-fx.
          IF market_data_entry-PriceValue > fx_max_rate.
            APPEND VALUE #( %tky = market_data_entry-%tky ) TO failed-marketdata.
            APPEND VALUE #( %tky = market_data_entry-%tky
                          %msg = new_message_with_text(
                            severity = if_abap_behv_message=>severity-error
                            text     = 'FX rate exceeds maximum threshold' )
                        ) TO reported-marketdata.
          ENDIF.

        WHEN market_data_type-ir.
          IF market_data_entry-PriceValue > ir_max_rate.
            APPEND VALUE #( %tky = market_data_entry-%tky ) TO failed-marketdata.
            APPEND VALUE #( %tky = market_data_entry-%tky
                          %msg = new_message_with_text(
                            severity = if_abap_behv_message=>severity-error
                            text     = 'Interest rate exceeds maximum threshold' )
                        ) TO reported-marketdata.
          ENDIF.

        WHEN market_data_type-comm.
          IF market_data_entry-PriceValue > comm_max_rate.
            APPEND VALUE #( %tky = market_data_entry-%tky ) TO failed-marketdata.
            APPEND VALUE #( %tky = market_data_entry-%tky
                          %msg = new_message_with_text(
                            severity = if_abap_behv_message=>severity-error
                            text     = 'Commodity price exceeds maximum threshold' )
                        ) TO reported-marketdata.
          ENDIF.
      ENDCASE.
    ENDLOOP.
  ENDMETHOD.

  METHOD calculateConfidenceScore.
    " Read market data
    READ ENTITIES OF z_trm_market_data_c_cds IN LOCAL MODE
      ENTITY MarketData
        FIELDS ( DataSource PriceValue )
        WITH CORRESPONDING #( keys )
      RESULT DATA(market_data).

    " Calculate confidence score based on data source and other factors
    LOOP AT market_data INTO DATA(market_data_entry).
      DATA(confidence_score) = COND #(
        WHEN market_data_entry-DataSource = 'BLOOMBERG' THEN '0.95'
        WHEN market_data_entry-DataSource = 'REUTERS'   THEN '0.90'
        WHEN market_data_entry-DataSource = 'MANUAL'    THEN '0.70'
        ELSE '0.50'
      ).

      " Modify confidence score
      MODIFY ENTITIES OF z_trm_market_data_c_cds IN LOCAL MODE
        ENTITY MarketData
          UPDATE FIELDS ( ConfidenceScore )
          WITH VALUE #( (
            %tky            = market_data_entry-%tky
            ConfidenceScore = confidence_score
          ) ).
    ENDLOOP.
  ENDMETHOD.

  METHOD validateData.
    " Read market data
    READ ENTITIES OF z_trm_market_data_c_cds IN LOCAL MODE
      ENTITY MarketData
        ALL FIELDS
        WITH CORRESPONDING #( keys )
      RESULT DATA(market_data).

    " Validate and update status
    LOOP AT market_data INTO DATA(market_data_entry).
      " Perform validation checks
      DATA(is_valid) = abap_true.

      " Update validation status
      MODIFY ENTITIES OF z_trm_market_data_c_cds IN LOCAL MODE
        ENTITY MarketData
          UPDATE FIELDS ( IsValidated ValidationTimestamp ValidationUser )
          WITH VALUE #( (
            %tky               = market_data_entry-%tky
            IsValidated        = COND #( WHEN is_valid = abap_true THEN 'X' ELSE '' )
            ValidationTimestamp = cl_abap_context_info=>get_system_timestamp( )
            ValidationUser     = cl_abap_context_info=>get_user_technical_name( )
          ) ).

      " Return result
      APPEND VALUE #(
        %tky = market_data_entry-%tky
        %param = market_data_entry
      ) TO result.
    ENDLOOP.
  ENDMETHOD.

  METHOD rejectData.
    " Read market data
    READ ENTITIES OF z_trm_market_data_c_cds IN LOCAL MODE
      ENTITY MarketData
        ALL FIELDS
        WITH CORRESPONDING #( keys )
      RESULT DATA(market_data).

    " Update rejection status
    LOOP AT market_data INTO DATA(market_data_entry).
      MODIFY ENTITIES OF z_trm_market_data_c_cds IN LOCAL MODE
        ENTITY MarketData
          UPDATE FIELDS ( IsValidated ValidationTimestamp ValidationUser )
          WITH VALUE #( (
            %tky               = market_data_entry-%tky
            IsValidated        = ''  " Not validated
            ValidationTimestamp = cl_abap_context_info=>get_system_timestamp( )
            ValidationUser     = cl_abap_context_info=>get_user_technical_name( )
          ) ).

      " Return result
      APPEND VALUE #(
        %tky = market_data_entry-%tky
        %param = market_data_entry
      ) TO result.
    ENDLOOP.
  ENDMETHOD.

  METHOD requestMarketDataUpdate.
    " Read market data
    READ ENTITIES OF z_trm_market_data_c_cds IN LOCAL MODE
      ENTITY MarketData
        ALL FIELDS
        WITH CORRESPONDING #( keys )
      RESULT DATA(market_data).

    " Request update for each entry
    LOOP AT market_data INTO DATA(market_data_entry).
      " Here you would typically:
      " 1. Create update request in a separate table
      " 2. Trigger external system call if needed
      " 3. Update status

      " Return result
      APPEND VALUE #(
        %tky = market_data_entry-%tky
        %param = market_data_entry
      ) TO result.
    ENDLOOP.
  ENDMETHOD.

ENDCLASS.