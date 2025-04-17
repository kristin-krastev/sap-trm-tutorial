CLASS zbp_trm_bank_account DEFINITION
  PUBLIC
  ABSTRACT
  FINAL
  FOR BEHAVIOR OF z_trm_bank_account_cds.

  PUBLIC SECTION.
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.

CLASS zbp_trm_bank_account IMPLEMENTATION.

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

ENDCLASS.