CLASS zbp_trm_bank_account DEFINITION
  PUBLIC
  ABSTRACT
  FINAL
  FOR BEHAVIOR OF z_trm_bank_account_cds.

  PUBLIC SECTION.
  PROTECTED SECTION.
  PRIVATE SECTION.
    METHODS:
      check_account_operations
        IMPORTING
          iv_account_uuid TYPE tfm_account_uuid
        RETURNING
          VALUE(rv_has_operations) TYPE abap_bool,

      convert_currency
        IMPORTING
          iv_amount           TYPE tfm_amount
          iv_from_currency    TYPE waers
          iv_to_currency      TYPE waers
          iv_exchange_date    TYPE datum
        RETURNING
          VALUE(rv_amount)    TYPE tfm_amount
        RAISING
          cx_tfm_currency_error,

      auto_match_transactions
        IMPORTING
          iv_account_uuid     TYPE tfm_account_uuid
          iv_statement_date   TYPE datum
        RETURNING
          VALUE(rt_matches)   TYPE tfm_matches_tab.
ENDCLASS.

CLASS zbp_trm_bank_account IMPLEMENTATION.

  METHOD convert_currency.
    " Currency conversion using Treasury rates
    SELECT SINGLE rate
      FROM tfm_exchange_rates
      WHERE from_currency = @iv_from_currency
        AND to_currency = @iv_to_currency
        AND valid_date <= @iv_exchange_date
        AND rate_type = 'M'  " Market rate
      ORDER BY valid_date DESCENDING
      INTO @DATA(exchange_rate).

    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE cx_tfm_currency_error
        MESSAGE ID 'TFM_CURRENCY'
        NUMBER '001'
        WITH iv_from_currency iv_to_currency.
    ENDIF.

    rv_amount = iv_amount * exchange_rate.
  ENDMETHOD.

  METHOD check_account_operations.
    " Check for pending operations
    SELECT SINGLE @abap_true
      FROM tfm_cash_flow
      WHERE account_uuid = @iv_account_uuid
        AND status = 'PENDING'
      INTO @rv_has_operations.
  ENDMETHOD.

  METHOD validateAccount.
    " Read instance data
    READ ENTITIES OF z_trm_bank_account_cds IN LOCAL MODE
      ENTITY BankAccount
        FIELDS ( AccountID BankID Country )
        WITH CORRESPONDING #( keys )
      RESULT DATA(bank_accounts).

    " Check each account
    LOOP AT bank_accounts INTO DATA(bank_account).
      " Check account ID format
      IF NOT matches( val = bank_account-AccountID
                     regex = '^[A-Z0-9]{10}$' ).  " Example: 10 chars alphanumeric
        APPEND VALUE #( %key = bank_account-%key ) TO failed-bankaccount.
        APPEND VALUE #( %key = bank_account-%key
                       %msg = new_message_with_text(
                         severity = if_abap_behv_message=>severity-error
                         text = 'Invalid account ID format' )
                     ) TO reported-bankaccount.
      ENDIF.

      " Validate bank exists
      SELECT SINGLE @abap_true
        FROM t012
        WHERE bankl = @bank_account-BankID
        INTO @DATA(bank_exists).

      IF bank_exists <> abap_true.
        APPEND VALUE #( %key = bank_account-%key ) TO failed-bankaccount.
        APPEND VALUE #( %key = bank_account-%key
                       %msg = new_message_with_text(
                         severity = if_abap_behv_message=>severity-error
                         text = 'Bank does not exist' )
                     ) TO reported-bankaccount.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.

  METHOD validateBalance.
    " Read instance data
    READ ENTITIES OF z_trm_bank_account_cds IN LOCAL MODE
      ENTITY BankAccount
        FIELDS ( CurrentBalance ProjectedBalance AccountType )
        WITH CORRESPONDING #( keys )
      RESULT DATA(bank_accounts).

    " Check each account
    LOOP AT bank_accounts INTO DATA(bank_account).
      " Validate minimum balance based on account type
      CASE bank_account-AccountType.
        WHEN 'CURRENT'.
          IF bank_account-CurrentBalance < -1000000.  " -1M limit for current accounts
            APPEND VALUE #( %key = bank_account-%key ) TO failed-bankaccount.
            APPEND VALUE #( %key = bank_account-%key
                          %msg = new_message_with_text(
                            severity = if_abap_behv_message=>severity-error
                            text = 'Current balance exceeds overdraft limit' )
                        ) TO reported-bankaccount.
          ENDIF.

        WHEN 'SAVINGS'.
          IF bank_account-CurrentBalance < 0.  " No negative balance for savings
            APPEND VALUE #( %key = bank_account-%key ) TO failed-bankaccount.
            APPEND VALUE #( %key = bank_account-%key
                          %msg = new_message_with_text(
                            severity = if_abap_behv_message=>severity-error
                            text = 'Savings account cannot have negative balance' )
                        ) TO reported-bankaccount.
          ENDIF.
      ENDCASE.
    ENDLOOP.
  ENDMETHOD.

  METHOD checkBankDetails.
    " Read instance data
    READ ENTITIES OF z_trm_bank_account_cds IN LOCAL MODE
      ENTITY BankAccount
        FIELDS ( BankID Country IBAN SWIFT )
        WITH CORRESPONDING #( keys )
      RESULT DATA(bank_accounts).

    " Check each account
    LOOP AT bank_accounts INTO DATA(bank_account).
      " Validate IBAN format (simplified check)
      IF NOT matches( val = bank_account-IBAN
                     regex = '^[A-Z]{2}[0-9]{2}[A-Z0-9]{12,30}$' ).
        APPEND VALUE #( %key = bank_account-%key ) TO failed-bankaccount.
        APPEND VALUE #( %key = bank_account-%key
                       %msg = new_message_with_text(
                         severity = if_abap_behv_message=>severity-error
                         text = 'Invalid IBAN format' )
                     ) TO reported-bankaccount.
      ENDIF.

      " Validate SWIFT code format
      IF NOT matches( val = bank_account-SWIFT
                     regex = '^[A-Z]{6}[A-Z0-9]{2}([A-Z0-9]{3})?$' ).
        APPEND VALUE #( %key = bank_account-%key ) TO failed-bankaccount.
        APPEND VALUE #( %key = bank_account-%key
                       %msg = new_message_with_text(
                         severity = if_abap_behv_message=>severity-error
                         text = 'Invalid SWIFT code format' )
                     ) TO reported-bankaccount.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.

  METHOD setAccountStatus.
    " Read instance data
    READ ENTITIES OF z_trm_bank_account_cds IN LOCAL MODE
      ENTITY BankAccount
        FIELDS ( AccountStatus LastStatementDate )
        WITH CORRESPONDING #( keys )
      RESULT DATA(bank_accounts).

    " Process each account
    LOOP AT bank_accounts INTO DATA(bank_account).
      " Calculate days since last statement
      DATA(days_since_statement) = cl_abap_context_info=>get_system_date( ) - bank_account-LastStatementDate.

      " Update status based on conditions
      DATA(new_status) = COND #(
        WHEN days_since_statement > 90 THEN 'INACTIVE'
        WHEN days_since_statement > 30 THEN 'WARNING'
        ELSE 'ACTIVE'
      ).

      " Update only if status changed
      IF new_status <> bank_account-AccountStatus.
        MODIFY ENTITIES OF z_trm_bank_account_cds IN LOCAL MODE
          ENTITY BankAccount
            UPDATE FIELDS ( AccountStatus )
            WITH VALUE #( (
              %key = bank_account-%key
              AccountStatus = new_status
            ) ).
      ENDIF.
    ENDLOOP.
  ENDMETHOD.

  METHOD calculateBalances.
    " Read instance data
    READ ENTITIES OF z_trm_bank_account_cds IN LOCAL MODE
      ENTITY BankAccount
        FIELDS ( AccountUUID CurrentBalance )
        WITH CORRESPONDING #( keys )
      RESULT DATA(bank_accounts).

    " Process each account
    LOOP AT bank_accounts INTO DATA(bank_account).
      " Calculate projected balance based on pending transactions
      SELECT SUM( flow_amount ) AS total_pending
        FROM tfm_cash_flow
        WHERE account_uuid = @bank_account-AccountUUID
          AND status = 'PENDING'
        INTO @DATA(pending_amount).

      " Update projected balance
      MODIFY ENTITIES OF z_trm_bank_account_cds IN LOCAL MODE
        ENTITY BankAccount
          UPDATE FIELDS ( ProjectedBalance )
          WITH VALUE #( (
            %key = bank_account-%key
            ProjectedBalance = bank_account-CurrentBalance + pending_amount
          ) ).
    ENDLOOP.
  ENDMETHOD.

  METHOD activateAccount.
    " Read current account data
    READ ENTITIES OF z_trm_bank_account_cds IN LOCAL MODE
      ENTITY BankAccount
        FIELDS ( AccountStatus )
        WITH CORRESPONDING #( keys )
      RESULT DATA(bank_accounts).

    " Process activation
    LOOP AT bank_accounts INTO DATA(bank_account).
      " Update account status
      MODIFY ENTITIES OF z_trm_bank_account_cds IN LOCAL MODE
        ENTITY BankAccount
          UPDATE FIELDS ( AccountStatus )
          WITH VALUE #( (
            %key = bank_account-%key
            AccountStatus = 'ACTIVE'
          ) ).

      " Return result
      APPEND VALUE #( %key = bank_account-%key
                     %msg = new_message_with_text(
                       severity = if_abap_behv_message=>severity-success
                       text = 'Account activated successfully' )
                   ) TO reported-bankaccount.
    ENDLOOP.
  ENDMETHOD.

  METHOD suspendAccount.
    " Read current account data
    READ ENTITIES OF z_trm_bank_account_cds IN LOCAL MODE
      ENTITY BankAccount
        FIELDS ( AccountUUID AccountStatus )
        WITH CORRESPONDING #( keys )
      RESULT DATA(bank_accounts).

    " Process suspension
    LOOP AT bank_accounts INTO DATA(bank_account).
      " Check if account can be suspended
      IF check_account_operations( bank_account-AccountUUID ) = abap_true.
        " Cannot suspend account with pending operations
        APPEND VALUE #( %key = bank_account-%key ) TO failed-bankaccount.
        APPEND VALUE #( %key = bank_account-%key
                       %msg = new_message_with_text(
                         severity = if_abap_behv_message=>severity-error
                         text = 'Cannot suspend account with pending operations' )
                     ) TO reported-bankaccount.
        CONTINUE.
      ENDIF.

      " Update account status
      MODIFY ENTITIES OF z_trm_bank_account_cds IN LOCAL MODE
        ENTITY BankAccount
          UPDATE FIELDS ( AccountStatus )
          WITH VALUE #( (
            %key = bank_account-%key
            AccountStatus = 'SUSPENDED'
          ) ).

      " Return result
      APPEND VALUE #( %key = bank_account-%key
                     %msg = new_message_with_text(
                       severity = if_abap_behv_message=>severity-success
                       text = 'Account suspended successfully' )
                   ) TO reported-bankaccount.
    ENDLOOP.
  ENDMETHOD.

  METHOD closeAccount.
    " Read current account data
    READ ENTITIES OF z_trm_bank_account_cds IN LOCAL MODE
      ENTITY BankAccount
        FIELDS ( AccountUUID AccountStatus CurrentBalance )
        WITH CORRESPONDING #( keys )
      RESULT DATA(bank_accounts).

    " Process closure
    LOOP AT bank_accounts INTO DATA(bank_account).
      " Check conditions for closure
      IF bank_account-CurrentBalance <> 0.
        APPEND VALUE #( %key = bank_account-%key ) TO failed-bankaccount.
        APPEND VALUE #( %key = bank_account-%key
                       %msg = new_message_with_text(
                         severity = if_abap_behv_message=>severity-error
                         text = 'Account must have zero balance to close' )
                     ) TO reported-bankaccount.
        CONTINUE.
      ENDIF.

      IF check_account_operations( bank_account-AccountUUID ) = abap_true.
        APPEND VALUE #( %key = bank_account-%key ) TO failed-bankaccount.
        APPEND VALUE #( %key = bank_account-%key
                       %msg = new_message_with_text(
                         severity = if_abap_behv_message=>severity-error
                         text = 'Cannot close account with pending operations' )
                     ) TO reported-bankaccount.
        CONTINUE.
      ENDIF.

      " Update account status
      MODIFY ENTITIES OF z_trm_bank_account_cds IN LOCAL MODE
        ENTITY BankAccount
          UPDATE FIELDS ( AccountStatus )
          WITH VALUE #( (
            %key = bank_account-%key
            AccountStatus = 'CLOSED'
          ) ).

      " Return result
      APPEND VALUE #( %key = bank_account-%key
                     %msg = new_message_with_text(
                       severity = if_abap_behv_message=>severity-success
                       text = 'Account closed successfully' )
                   ) TO reported-bankaccount.
    ENDLOOP.
  ENDMETHOD.

  METHOD updateBalance.
    " Read current account data
    READ ENTITIES OF z_trm_bank_account_cds IN LOCAL MODE
      ENTITY BankAccount
        FIELDS ( AccountUUID AccountStatus CurrentBalance AccountCurrency )
        WITH CORRESPONDING #( keys )
      RESULT DATA(bank_accounts).

    " Process balance update
    LOOP AT bank_accounts INTO DATA(bank_account).
      " Check if account is active
      IF bank_account-AccountStatus <> 'ACTIVE'.
        APPEND VALUE #( %key = bank_account-%key ) TO failed-bankaccount.
        APPEND VALUE #( %key = bank_account-%key
                       %msg = new_message_with_text(
                         severity = if_abap_behv_message=>severity-error
                         text = 'Balance can only be updated for active accounts' )
                     ) TO reported-bankaccount.
        CONTINUE.
      ENDIF.

      " Read update parameters
      DATA(ls_params) = VALUE z_trm_balance_update(
        amount = parameters-amount
        currency = parameters-currency
        value_date = parameters-value_date
        transaction_type = parameters-transaction_type
        reference = parameters-reference
      ).

      " Validate parameters
      IF ls_params-amount IS INITIAL OR
         ls_params-currency IS INITIAL OR
         ls_params-value_date IS INITIAL OR
         ls_params-transaction_type IS INITIAL.
        APPEND VALUE #( %key = bank_account-%key ) TO failed-bankaccount.
        APPEND VALUE #( %key = bank_account-%key
                       %msg = new_message_with_text(
                         severity = if_abap_behv_message=>severity-error
                         text = 'All update parameters are required' )
                     ) TO reported-bankaccount.
        CONTINUE.
      ENDIF.

      TRY.
          " Convert amount if currencies differ
          DATA(lv_converted_amount) = COND #(
            WHEN ls_params-currency = bank_account-AccountCurrency
            THEN ls_params-amount
            ELSE convert_currency(
              iv_amount        = ls_params-amount
              iv_from_currency = ls_params-currency
              iv_to_currency   = bank_account-AccountCurrency
              iv_exchange_date = ls_params-value_date
            )
          ).

          " Create cash flow entry
          MODIFY ENTITIES OF z_trm_cashflow_cds IN LOCAL MODE
            ENTITY CashFlow
              CREATE FIELDS (
                FlowUUID AccountUUID FlowType Amount
                Currency ValueDate Reference Status
              )
              WITH VALUE #( (
                FlowUUID     = cl_system_uuid=>create_uuid_x16_static( )
                AccountUUID  = bank_account-AccountUUID
                FlowType     = ls_params-transaction_type
                Amount      = lv_converted_amount
                Currency    = bank_account-AccountCurrency
                ValueDate   = ls_params-value_date
                Reference   = ls_params-reference
                Status      = 'POSTED'
              ) ).

          " Update account balance
          MODIFY ENTITIES OF z_trm_bank_account_cds IN LOCAL MODE
            ENTITY BankAccount
              UPDATE FIELDS ( CurrentBalance LastChangedAt )
              WITH VALUE #( (
                %key = bank_account-%key
                CurrentBalance = bank_account-CurrentBalance + lv_converted_amount
                LastChangedAt = cl_abap_context_info=>get_system_timestamp( )
              ) ).

          " Return success message
          APPEND VALUE #( %key = bank_account-%key
                         %msg = new_message_with_text(
                           severity = if_abap_behv_message=>severity-success
                           text = |Balance updated successfully. New balance: { bank_account-CurrentBalance + lv_converted_amount }| )
                       ) TO reported-bankaccount.

        CATCH cx_tfm_currency_error INTO DATA(lx_currency_error).
          APPEND VALUE #( %key = bank_account-%key ) TO failed-bankaccount.
          APPEND VALUE #( %key = bank_account-%key
                         %msg = new_message_with_text(
                           severity = if_abap_behv_message=>severity-error
                           text = lx_currency_error->get_text( ) )
                       ) TO reported-bankaccount.
      ENDTRY.
    ENDLOOP.
  ENDMETHOD.

  METHOD reconcileStatement.
    " Read current account data
    READ ENTITIES OF z_trm_bank_account_cds IN LOCAL MODE
      ENTITY BankAccount
        FIELDS ( AccountUUID AccountStatus CurrentBalance AccountCurrency LastStatementDate )
        WITH CORRESPONDING #( keys )
      RESULT DATA(bank_accounts).

    " Process reconciliation
    LOOP AT bank_accounts INTO DATA(bank_account).
      " Check if account is active
      IF bank_account-AccountStatus <> 'ACTIVE'.
        APPEND VALUE #( %key = bank_account-%key ) TO failed-bankaccount.
        APPEND VALUE #( %key = bank_account-%key
                       %msg = new_message_with_text(
                         severity = if_abap_behv_message=>severity-error
                         text = 'Statement can only be reconciled for active accounts' )
                     ) TO reported-bankaccount.
        CONTINUE.
      ENDIF.

      " Read reconciliation parameters
      DATA(ls_params) = VALUE z_trm_statement_recon(
        statement_id = parameters-statement_id
        statement_date = parameters-statement_date
        closing_balance = parameters-closing_balance
        currency = parameters-currency
        auto_match = parameters-auto_match
      ).

      " Validate parameters
      IF ls_params-statement_date IS INITIAL OR
         ls_params-closing_balance IS INITIAL OR
         ls_params-currency IS INITIAL.
        APPEND VALUE #( %key = bank_account-%key ) TO failed-bankaccount.
        APPEND VALUE #( %key = bank_account-%key
                       %msg = new_message_with_text(
                         severity = if_abap_behv_message=>severity-error
                         text = 'All reconciliation parameters are required' )
                     ) TO reported-bankaccount.
        CONTINUE.
      ENDIF.

      " Check statement date sequence
      IF ls_params-statement_date <= bank_account-LastStatementDate.
        APPEND VALUE #( %key = bank_account-%key ) TO failed-bankaccount.
        APPEND VALUE #( %key = bank_account-%key
                       %msg = new_message_with_text(
                         severity = if_abap_behv_message=>severity-error
                         text = 'Statement date must be after last reconciliation' )
                     ) TO reported-bankaccount.
        CONTINUE.
      ENDIF.

      TRY.
          " Convert closing balance if needed
          DATA(lv_converted_balance) = COND #(
            WHEN ls_params-currency = bank_account-AccountCurrency
            THEN ls_params-closing_balance
            ELSE convert_currency(
              iv_amount        = ls_params-closing_balance
              iv_from_currency = ls_params-currency
              iv_to_currency   = bank_account-AccountCurrency
              iv_exchange_date = ls_params-statement_date
            )
          ).

          " Perform auto-matching if requested
          IF ls_params-auto_match = abap_true.
            DATA(lt_matches) = auto_match_transactions(
              iv_account_uuid   = bank_account-AccountUUID
              iv_statement_date = ls_params-statement_date
            ).
          ENDIF.

          " Calculate difference
          DATA(lv_difference) = lv_converted_balance - bank_account-CurrentBalance.

          " Create reconciliation record
          MODIFY ENTITIES OF z_trm_statement_recon_cds IN LOCAL MODE
            ENTITY StatementRecon
              CREATE FIELDS (
                ReconUUID StatementID AccountUUID StatementDate
                ClosingBalance Currency Difference Status
              )
              WITH VALUE #( (
                ReconUUID = cl_system_uuid=>create_uuid_x16_static( )
                StatementID = ls_params-statement_id
                AccountUUID = bank_account-AccountUUID
                StatementDate = ls_params-statement_date
                ClosingBalance = lv_converted_balance
                Currency = bank_account-AccountCurrency
                Difference = lv_difference
                Status = COND #( WHEN lv_difference = 0 THEN 'MATCHED'
                                ELSE 'UNMATCHED' )
              ) ).

          " Update account last statement date
          MODIFY ENTITIES OF z_trm_bank_account_cds IN LOCAL MODE
            ENTITY BankAccount
              UPDATE FIELDS ( LastStatementDate LastChangedAt )
              WITH VALUE #( (
                %key = bank_account-%key
                LastStatementDate = ls_params-statement_date
                LastChangedAt = cl_abap_context_info=>get_system_timestamp( )
              ) ).

          " Return result message
          APPEND VALUE #( %key = bank_account-%key
                         %msg = new_message_with_text(
                           severity = COND #( WHEN lv_difference = 0
                                            THEN if_abap_behv_message=>severity-success
                                            ELSE if_abap_behv_message=>severity-warning )
                           text = COND #( WHEN lv_difference = 0
                                        THEN 'Statement reconciled successfully'
                                        ELSE |Statement reconciled with difference: { lv_difference }| )
                         )
                       ) TO reported-bankaccount.

        CATCH cx_tfm_currency_error INTO DATA(lx_currency_error).
          APPEND VALUE #( %key = bank_account-%key ) TO failed-bankaccount.
          APPEND VALUE #( %key = bank_account-%key
                         %msg = new_message_with_text(
                           severity = if_abap_behv_message=>severity-error
                           text = lx_currency_error->get_text( ) )
                       ) TO reported-bankaccount.
      ENDTRY.
    ENDLOOP.
  ENDMETHOD.

  METHOD auto_match_transactions.
    " Get unmatched transactions
    SELECT FROM tfm_cash_flow
      FIELDS flow_uuid, flow_amount, value_date, reference
      WHERE account_uuid = @iv_account_uuid
        AND status = 'POSTED'
        AND value_date <= @iv_statement_date
        AND matched = @abap_false
      INTO TABLE @DATA(lt_transactions).

    " Get statement entries
    SELECT FROM tfm_bank_stmt
      FIELDS stmt_entry_uuid, entry_amount, entry_date, reference
      WHERE account_uuid = @iv_account_uuid
        AND entry_date <= @iv_statement_date
        AND matched = @abap_false
      INTO TABLE @DATA(lt_stmt_entries).

    " Perform matching based on amount and reference
    LOOP AT lt_transactions INTO DATA(ls_trans).
      READ TABLE lt_stmt_entries INTO DATA(ls_stmt)
        WITH KEY entry_amount = ls_trans-flow_amount
                 reference   = ls_trans-reference.
      IF sy-subrc = 0.
        " Add to matches table
        APPEND VALUE #(
          flow_uuid      = ls_trans-flow_uuid
          stmt_entry_uuid = ls_stmt-stmt_entry_uuid
          match_type     = 'AUTO'
          match_date     = sy-datum
        ) TO rt_matches.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.

ENDCLASS.