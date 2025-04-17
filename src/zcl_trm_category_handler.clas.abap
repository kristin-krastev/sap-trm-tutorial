CLASS zcl_trm_category_handler DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.
    TYPES:
      BEGIN OF ty_category_hierarchy,
        category_id     TYPE tfm_category_id,
        parent_id      TYPE tfm_category_id,
        description    TYPE tfm_text,
        level          TYPE int4,
        has_children   TYPE abap_bool,
        risk_category  TYPE tfm_risk_cat,
        posting_key    TYPE bschl,
        gl_account     TYPE saknr,
      END OF ty_category_hierarchy,

      tt_category_hierarchy TYPE STANDARD TABLE OF ty_category_hierarchy WITH KEY category_id.

    METHODS:
      constructor,

      get_category_hierarchy
        IMPORTING
          iv_parent_id TYPE tfm_category_id OPTIONAL
        RETURNING
          VALUE(rt_hierarchy) TYPE tt_category_hierarchy
        RAISING
          cx_tfm_category_error,

      create_category
        IMPORTING
          is_category TYPE z_trm_trans_category
        RETURNING
          VALUE(rv_category_id) TYPE tfm_category_id
        RAISING
          cx_tfm_category_error,

      update_category
        IMPORTING
          is_category TYPE z_trm_trans_category
        RAISING
          cx_tfm_category_error,

      delete_category
        IMPORTING
          iv_category_id TYPE tfm_category_id
        RAISING
          cx_tfm_category_error.

  PROTECTED SECTION.
  PRIVATE SECTION.
    METHODS:
      validate_category
        IMPORTING
          is_category TYPE z_trm_trans_category
        RAISING
          cx_tfm_category_error,

      check_circular_reference
        IMPORTING
          iv_category_id     TYPE tfm_category_id
          iv_parent_id      TYPE tfm_category_id
        RETURNING
          VALUE(rv_is_circular) TYPE abap_bool,

      get_child_categories
        IMPORTING
          iv_category_id TYPE tfm_category_id
        RETURNING
          VALUE(rt_children) TYPE STANDARD TABLE OF tfm_category_id.
ENDCLASS.

CLASS zcl_trm_category_handler IMPLEMENTATION.

  METHOD constructor.
    " Initialize any necessary resources
  ENDMETHOD.

  METHOD get_category_hierarchy.
    DATA: lt_categories TYPE STANDARD TABLE OF z_trm_trans_category,
          lt_result    TYPE tt_category_hierarchy.

    " Get all active categories
    SELECT *
      FROM z_trm_trans_category
      WHERE active = @abap_true
      INTO TABLE @lt_categories.

    " Build hierarchy starting from root or specified parent
    DATA(lv_current_level) = 1.
    DATA(lv_parent) = iv_parent_id.

    WHILE lines( lt_categories ) > 0.
      LOOP AT lt_categories INTO DATA(ls_category) WHERE parent_category = lv_parent.
        " Get children count
        SELECT COUNT(*)
          FROM z_trm_trans_category
          WHERE parent_category = @ls_category-category_id
            AND active = @abap_true
          INTO @DATA(lv_children_count).

        " Add to hierarchy
        APPEND VALUE #(
          category_id    = ls_category-category_id
          parent_id     = ls_category-parent_category
          description   = ls_category-description
          level         = lv_current_level
          has_children  = xsdbool( lv_children_count > 0 )
          risk_category = ls_category-risk_category
          posting_key   = ls_category-posting_key
          gl_account    = ls_category-gl_account
        ) TO lt_result.

        " Remove processed category
        DELETE lt_categories WHERE category_id = ls_category-category_id.
      ENDLOOP.

      " Move to next level
      lv_current_level = lv_current_level + 1.
    ENDWHILE.

    rt_hierarchy = lt_result.
  ENDMETHOD.

  METHOD create_category.
    " Validate category data
    validate_category( is_category ).

    " Check for circular references
    IF check_circular_reference(
         iv_category_id = is_category-category_id
         iv_parent_id  = is_category-parent_category
       ).
      RAISE EXCEPTION TYPE cx_tfm_category_error
        MESSAGE ID 'TFM_CATEGORY'
        NUMBER '001'
        WITH is_category-category_id.
    ENDIF.

    " Generate category ID if not provided
    DATA(ls_category) = is_category.
    IF ls_category-category_id IS INITIAL.
      SELECT MAX( category_id )
        FROM z_trm_trans_category
        INTO @DATA(lv_max_id).
      ls_category-category_id = lv_max_id + 1.
    ENDIF.

    " Insert category
    INSERT z_trm_trans_category FROM ls_category.
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE cx_tfm_category_error
        MESSAGE ID 'TFM_CATEGORY'
        NUMBER '002'
        WITH ls_category-category_id.
    ENDIF.

    rv_category_id = ls_category-category_id.
  ENDMETHOD.

  METHOD update_category.
    " Validate category data
    validate_category( is_category ).

    " Check for circular references
    IF check_circular_reference(
         iv_category_id = is_category-category_id
         iv_parent_id  = is_category-parent_category
       ).
      RAISE EXCEPTION TYPE cx_tfm_category_error
        MESSAGE ID 'TFM_CATEGORY'
        NUMBER '001'
        WITH is_category-category_id.
    ENDIF.

    " Update category
    UPDATE z_trm_trans_category FROM is_category.
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE cx_tfm_category_error
        MESSAGE ID 'TFM_CATEGORY'
        NUMBER '003'
        WITH is_category-category_id.
    ENDIF.
  ENDMETHOD.

  METHOD delete_category.
    " Check for child categories
    DATA(lt_children) = get_child_categories( iv_category_id ).
    IF lines( lt_children ) > 0.
      RAISE EXCEPTION TYPE cx_tfm_category_error
        MESSAGE ID 'TFM_CATEGORY'
        NUMBER '004'
        WITH iv_category_id.
    ENDIF.

    " Soft delete by setting active = false
    UPDATE z_trm_trans_category
      SET active = @abap_false
      WHERE category_id = @iv_category_id.

    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE cx_tfm_category_error
        MESSAGE ID 'TFM_CATEGORY'
        NUMBER '005'
        WITH iv_category_id.
    ENDIF.
  ENDMETHOD.

  METHOD validate_category.
    " Check mandatory fields
    IF is_category-description IS INITIAL.
      RAISE EXCEPTION TYPE cx_tfm_category_error
        MESSAGE ID 'TFM_CATEGORY'
        NUMBER '006'.
    ENDIF.

    " Validate posting key
    SELECT SINGLE @abap_true
      FROM t003
      WHERE blart = @is_category-posting_key
      INTO @DATA(lv_posting_key_valid).

    IF lv_posting_key_valid <> abap_true.
      RAISE EXCEPTION TYPE cx_tfm_category_error
        MESSAGE ID 'TFM_CATEGORY'
        NUMBER '007'
        WITH is_category-posting_key.
    ENDIF.

    " Validate GL account
    SELECT SINGLE @abap_true
      FROM ska1
      WHERE saknr = @is_category-gl_account
      INTO @DATA(lv_gl_account_valid).

    IF lv_gl_account_valid <> abap_true.
      RAISE EXCEPTION TYPE cx_tfm_category_error
        MESSAGE ID 'TFM_CATEGORY'
        NUMBER '008'
        WITH is_category-gl_account.
    ENDIF.
  ENDMETHOD.

  METHOD check_circular_reference.
    DATA: lt_parents TYPE STANDARD TABLE OF tfm_category_id.
    DATA(lv_current_id) = iv_parent_id.

    WHILE lv_current_id IS NOT INITIAL.
      " Check if we've seen this ID before
      IF line_exists( lt_parents[ table_line = lv_current_id ] ).
        rv_is_circular = abap_true.
        RETURN.
      ENDIF.

      " Add to parents list
      APPEND lv_current_id TO lt_parents.

      " Get next parent
      SELECT SINGLE parent_category
        FROM z_trm_trans_category
        WHERE category_id = @lv_current_id
        INTO @lv_current_id.
    ENDWHILE.

    rv_is_circular = xsdbool( iv_category_id IN lt_parents ).
  ENDMETHOD.

  METHOD get_child_categories.
    SELECT category_id
      FROM z_trm_trans_category
      WHERE parent_category = @iv_category_id
        AND active = @abap_true
      INTO TABLE @rt_children.
  ENDMETHOD.

ENDCLASS.