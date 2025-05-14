METHOD get_instance_authorizations.
  LOOP AT keys ASSIGNING FIELD-SYMBOL(<key>).
    APPEND VALUE #( %tky = <key>-%tky
                   %create = if_abap_behv=>auth-allowed
                   %update = if_abap_behv=>auth-allowed
                   %delete = if_abap_behv=>auth-allowed
                   %action-Edit = if_abap_behv=>auth-allowed ) TO result.
  ENDLOOP.
ENDMETHOD.

METHOD setInitialStatus.
  " Read position instances
  READ ENTITIES OF zkkr_i_position IN LOCAL MODE
    ENTITY Position
      FIELDS ( Status )
      WITH CORRESPONDING #( keys )
    RESULT DATA(positions).

  " Modify position instances
  MODIFY ENTITIES OF zkkr_i_position IN LOCAL MODE
    ENTITY Position
      UPDATE
        FIELDS ( Status )
        WITH VALUE #( FOR position IN positions
                     ( %tky          = position-%tky
                       Status = 'O' ) ).  "O = Open
ENDMETHOD.