CLASS zbp_trm_limit DEFINITION
  PUBLIC
  ABSTRACT
  FINAL
  FOR BEHAVIOR OF z_trm_limit_c_cds.

  PUBLIC SECTION.
    CONSTANTS:
      BEGIN OF limit_type,
        counterparty TYPE c LENGTH 4 VALUE 'CPTY',
        market      TYPE c LENGTH 3 VALUE 'MKT',
        country     TYPE c LENGTH 3 VALUE 'CTY',
      END OF limit_type,

      BEGIN OF limit_category,
        trading    TYPE c LENGTH 7 VALUE 'TRADING',
        settlement TYPE c LENGTH 10 VALUE 'SETTLEMENT',
        total      TYPE c LENGTH 5 VALUE 'TOTAL',
      END OF limit_category,

      BEGIN OF approval_status,
        pending  TYPE c LENGTH 7 VALUE 'PENDING',
        approved TYPE c LENGTH 8 VALUE 'APPROVED',
        rejected TYPE c LENGTH 8 VALUE 'REJECTED',
      END OF approval_status.

  PRIVATE SECTION.
    METHODS:
      validateLimit FOR VALIDATE ON SAVE
        IMPORTING keys FOR Limit~validateLimit,

      validateDates FOR VALIDATE ON SAVE
        IMPORTING keys FOR Limit~validateDates,

      validateAmount FOR VALIDATE ON SAVE
        IMPORTING keys FOR Limit~validateAmount,

      checkApprovalAuthority FOR VALIDATE ON SAVE
        IMPORTING keys FOR Limit~checkApprovalAuthority,

      calculateUtilization FOR DETERMINE ON MODIFY
        IMPORTING keys FOR Limit~calculateUtilization,

      approveLimitChange FOR MODIFY
        IMPORTING keys FOR ACTION Limit~approveLimitChange RESULT result,

      rejectLimitChange FOR MODIFY
        IMPORTING keys FOR ACTION Limit~rejectLimitChange RESULT result,

      requestLimitIncrease FOR MODIFY
        IMPORTING keys FOR ACTION Limit~requestLimitIncrease RESULT result,

      resetUtilization FOR MODIFY
        IMPORTING keys FOR ACTION Limit~resetUtilization RESULT result.

ENDCLASS.

CLASS zbp_trm_limit IMPLEMENTATION.

  METHOD validateLimit.
    " Read limit data
    READ ENTITIES OF z_trm_limit_c_cds IN LOCAL MODE
      ENTITY Limit
        FIELDS ( LimitType LimitCategory ObjectID )
        WITH CORRESPONDING #( keys )
      RESULT DATA(limits).

    " Validate each entry
    LOOP AT limits INTO DATA(limit).
      " Check limit type
      IF limit-LimitType NE limit_type-counterparty AND
         limit-LimitType NE limit_type-market AND
         limit-LimitType NE limit_type-country.
        APPEND VALUE #( %tky = limit-%tky ) TO failed-limit.
        APPEND VALUE #( %tky = limit-%tky
                       %msg = new_message_with_text(
                         severity = if_abap_behv_message=>severity-error
                         text     = 'Invalid limit type' )
                     ) TO reported-limit.
        CONTINUE.
      ENDIF.

      " Check limit category
      IF limit-LimitCategory NE limit_category-trading AND
         limit-LimitCategory NE limit_category-settlement AND
         limit-LimitCategory NE limit_category-total.
        APPEND VALUE #( %tky = limit-%tky ) TO failed-limit.
        APPEND VALUE #( %tky = limit-%tky
                       %msg = new_message_with_text(
                         severity = if_abap_behv_message=>severity-error
                         text     = 'Invalid limit category' )
                     ) TO reported-limit.
      ENDIF.

      " Validate object ID format based on type
      CASE limit-LimitType.
        WHEN limit_type-counterparty.
          " Check if counterparty exists
          " This would typically involve a check against business partner table
          " For demo purposes, we'll just check if it's not initial
          IF limit-ObjectID IS INITIAL.
            APPEND VALUE #( %tky = limit-%tky ) TO failed-limit.
            APPEND VALUE #( %tky = limit-%tky
                          %msg = new_message_with_text(
                            severity = if_abap_behv_message=>severity-error
                            text     = 'Invalid counterparty ID' )
                        ) TO reported-limit.
          ENDIF.
      ENDCASE.
    ENDLOOP.
  ENDMETHOD.

  METHOD validateDates.
    READ ENTITIES OF z_trm_limit_c_cds IN LOCAL MODE
      ENTITY Limit
        FIELDS ( ValidFrom ValidTo )
        WITH CORRESPONDING #( keys )
      RESULT DATA(limits).

    LOOP AT limits INTO DATA(limit).
      " Check if ValidFrom is in the past
      IF limit-ValidFrom < cl_abap_context_info=>get_system_date( ).
        APPEND VALUE #( %tky = limit-%tky ) TO failed-limit.
        APPEND VALUE #( %tky = limit-%tky
                       %msg = new_message_with_text(
                         severity = if_abap_behv_message=>severity-error
                         text     = 'Valid from date cannot be in the past' )
                     ) TO reported-limit.
      ENDIF.

      " Check if ValidTo is after ValidFrom
      IF limit-ValidTo IS NOT INITIAL AND
         limit-ValidTo <= limit-ValidFrom.
        APPEND VALUE #( %tky = limit-%tky ) TO failed-limit.
        APPEND VALUE #( %tky = limit-%tky
                       %msg = new_message_with_text(
                         severity = if_abap_behv_message=>severity-error
                         text     = 'Valid to date must be after valid from date' )
                     ) TO reported-limit.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.

  METHOD validateAmount.
    READ ENTITIES OF z_trm_limit_c_cds IN LOCAL MODE
      ENTITY Limit
        FIELDS ( LimitAmount UtilizedAmount )
        WITH CORRESPONDING #( keys )
      RESULT DATA(limits).

    LOOP AT limits INTO DATA(limit).
      " Check if limit amount is positive
      IF limit-LimitAmount <= 0.
        APPEND VALUE #( %tky = limit-%tky ) TO failed-limit.
        APPEND VALUE #( %tky = limit-%tky
                       %msg = new_message_with_text(
                         severity = if_abap_behv_message=>severity-error
                         text     = 'Limit amount must be positive' )
                     ) TO reported-limit.
      ENDIF.

      " Check if utilized amount is non-negative
      IF limit-UtilizedAmount < 0.
        APPEND VALUE #( %tky = limit-%tky ) TO failed-limit.
        APPEND VALUE #( %tky = limit-%tky
                       %msg = new_message_with_text(
                         severity = if_abap_behv_message=>severity-error
                         text     = 'Utilized amount cannot be negative' )
                     ) TO reported-limit.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.

  METHOD checkApprovalAuthority.
    " Read limit data
    READ ENTITIES OF z_trm_limit_c_cds IN LOCAL MODE
      ENTITY Limit
        FIELDS ( LimitAmount ApproverID )
        WITH CORRESPONDING #( keys )
      RESULT DATA(limits).

    " In a real implementation, you would check against an authority table
    " For demo purposes, we'll just check if approver is assigned for large amounts
    LOOP AT limits INTO DATA(limit).
      IF limit-LimitAmount >= 1000000 AND limit-ApproverID IS INITIAL.
        APPEND VALUE #( %tky = limit-%tky ) TO failed-limit.
        APPEND VALUE #( %tky = limit-%tky
                       %msg = new_message_with_text(
                         severity = if_abap_behv_message=>severity-error
                         text     = 'Approver required for large limits' )
                     ) TO reported-limit.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.

  METHOD calculateUtilization.
    " Read limit data
    READ ENTITIES OF z_trm_limit_c_cds IN LOCAL MODE
      ENTITY Limit
        FIELDS ( LimitAmount UtilizedAmount )
        WITH CORRESPONDING #( keys )
      RESULT DATA(limits).

    " Calculate utilization for each limit
    LOOP AT limits INTO DATA(limit).
      " Here you would typically:
      " 1. Calculate current utilization from positions
      " 2. Update the utilized amount
      " For demo purposes, we'll just ensure utilized amount doesn't exceed limit

      IF limit-UtilizedAmount > limit-LimitAmount.
        MODIFY ENTITIES OF z_trm_limit_c_cds IN LOCAL MODE
          ENTITY Limit
            UPDATE FIELDS ( UtilizedAmount )
            WITH VALUE #( (
              %tky           = limit-%tky
              UtilizedAmount = limit-LimitAmount
            ) ).
      ENDIF.
    ENDLOOP.
  ENDMETHOD.

  METHOD approveLimitChange.
    " Read limit data
    READ ENTITIES OF z_trm_limit_c_cds IN LOCAL MODE
      ENTITY Limit
        ALL FIELDS
        WITH CORRESPONDING #( keys )
      RESULT DATA(limits).

    " Update approval status
    LOOP AT limits INTO DATA(limit).
      MODIFY ENTITIES OF z_trm_limit_c_cds IN LOCAL MODE
        ENTITY Limit
          UPDATE FIELDS ( ApprovalStatus ApprovalTimestamp ApproverID )
          WITH VALUE #( (
            %tky              = limit-%tky
            ApprovalStatus    = approval_status-approved
            ApprovalTimestamp = cl_abap_context_info=>get_system_timestamp( )
            ApproverID        = cl_abap_context_info=>get_user_technical_name( )
          ) ).

      " Return result
      APPEND VALUE #(
        %tky = limit-%tky
        %param = limit
      ) TO result.
    ENDLOOP.
  ENDMETHOD.

  METHOD rejectLimitChange.
    " Read limit data
    READ ENTITIES OF z_trm_limit_c_cds IN LOCAL MODE
      ENTITY Limit
        ALL FIELDS
        WITH CORRESPONDING #( keys )
      RESULT DATA(limits).

    " Update rejection status
    LOOP AT limits INTO DATA(limit).
      MODIFY ENTITIES OF z_trm_limit_c_cds IN LOCAL MODE
        ENTITY Limit
          UPDATE FIELDS ( ApprovalStatus ApprovalTimestamp ApproverID )
          WITH VALUE #( (
            %tky              = limit-%tky
            ApprovalStatus    = approval_status-rejected
            ApprovalTimestamp = cl_abap_context_info=>get_system_timestamp( )
            ApproverID        = cl_abap_context_info=>get_user_technical_name( )
          ) ).

      " Return result
      APPEND VALUE #(
        %tky = limit-%tky
        %param = limit
      ) TO result.
    ENDLOOP.
  ENDMETHOD.

  METHOD requestLimitIncrease.
    " Read limit data
    READ ENTITIES OF z_trm_limit_c_cds IN LOCAL MODE
      ENTITY Limit
        ALL FIELDS
        WITH CORRESPONDING #( keys )
      RESULT DATA(limits).

    " Process limit increase request
    LOOP AT limits INTO DATA(limit).
      " Here you would typically:
      " 1. Create increase request record
      " 2. Trigger workflow
      " 3. Update status

      " For demo, we'll just set it to pending approval
      MODIFY ENTITIES OF z_trm_limit_c_cds IN LOCAL MODE
        ENTITY Limit
          UPDATE FIELDS ( ApprovalStatus )
          WITH VALUE #( (
            %tky           = limit-%tky
            ApprovalStatus = approval_status-pending
          ) ).

      " Return result
      APPEND VALUE #(
        %tky = limit-%tky
        %param = limit
      ) TO result.
    ENDLOOP.
  ENDMETHOD.

  METHOD resetUtilization.
    " Read limit data
    READ ENTITIES OF z_trm_limit_c_cds IN LOCAL MODE
      ENTITY Limit
        ALL FIELDS
        WITH CORRESPONDING #( keys )
      RESULT DATA(limits).

    " Reset utilization
    LOOP AT limits INTO DATA(limit).
      MODIFY ENTITIES OF z_trm_limit_c_cds IN LOCAL MODE
        ENTITY Limit
          UPDATE FIELDS ( UtilizedAmount )
          WITH VALUE #( (
            %tky           = limit-%tky
            UtilizedAmount = 0
          ) ).

      " Return result
      APPEND VALUE #(
        %tky = limit-%tky
        %param = limit
      ) TO result.
    ENDLOOP.
  ENDMETHOD.

ENDCLASS.