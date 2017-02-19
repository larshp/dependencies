REPORT zdependencies.

* todo, handle cyclic dependencies
* todo: search helps? from TABL and DTEL?
* todo: check tables from DOMA?

PARAMETERS: p_type TYPE tadir-object OBLIGATORY DEFAULT 'PROG',
            p_name TYPE tadir-obj_name OBLIGATORY DEFAULT 'ZABAPGIT'.


TYPES: BEGIN OF ty_item,
           obj_type TYPE tadir-object,
           obj_name TYPE tadir-obj_name,
         END OF ty_item.

TYPES: BEGIN OF ty_result,
         type  TYPE tadir-object,
         name  TYPE tadir-obj_name,
         level TYPE i,
       END OF ty_result.

TYPES: ty_result_tt TYPE STANDARD TABLE OF ty_result WITH DEFAULT KEY.

CLASS lcl_utils DEFINITION.
  PUBLIC SECTION.
    CLASS-METHODS:
      resolve
        IMPORTING
          iv_name TYPE clike
        RETURNING
          VALUE(rv_type) TYPE tadir-object,
      resolve_func
        IMPORTING
          iv_name TYPE cross-name
        RETURNING
          VALUE(rv_fugr) TYPE tadir-object.
ENDCLASS.

CLASS lcl_utils IMPLEMENTATION.

  METHOD resolve.

    DATA: lv_clstype TYPE seoclass-clstype.


    SELECT SINGLE COUNT(*) FROM dd04l WHERE rollname = iv_name.
    IF sy-subrc = 0.
      rv_type = 'DTEL'.
      RETURN.
    ENDIF.

    SELECT SINGLE COUNT(*) FROM dd02l WHERE tabname = iv_name.
    IF sy-subrc = 0.
      rv_type = 'TABL'.
      RETURN.
    ENDIF.

    SELECT SINGLE clstype FROM seoclass INTO lv_clstype WHERE clsname = iv_name.
    IF sy-subrc = 0.
      CASE lv_clstype.
        WHEN '0'.
          rv_type = 'CLAS'.
          RETURN.
        WHEN '1'.
          rv_type = 'INTF'.
          RETURN.
        WHEN OTHERS.
          ASSERT 0 = 1.
      ENDCASE.
    ENDIF.

    SELECT SINGLE COUNT(*) FROM dd40l WHERE typename = iv_name.
    IF sy-subrc = 0.
      rv_type = 'TTYP'.
      RETURN.
    ENDIF.

    BREAK-POINT.
    rv_type = 'TODO'.

  ENDMETHOD.

  METHOD resolve_func.

    DATA: ls_tfdir TYPE tfdir.

    SELECT SINGLE * FROM tfdir INTO ls_tfdir WHERE funcname = iv_name.
    IF sy-subrc <> 0.
      BREAK-POINT.
    ENDIF.

    rv_fugr = ls_tfdir-pname_main+4.

  ENDMETHOD.

ENDCLASS.

INTERFACE lif_object.
  METHODS: find_dependencies RETURNING VALUE(rt_result) TYPE ty_result_tt.
ENDINTERFACE.

CLASS lcl_objects_super DEFINITION.
  PUBLIC SECTION.
    METHODS: constructor IMPORTING is_item TYPE ty_item.
  PROTECTED SECTION.
    DATA: ms_item TYPE ty_item.
ENDCLASS.
CLASS lcl_objects_super IMPLEMENTATION.
  METHOD constructor.
    ms_item = is_item.
  ENDMETHOD.
ENDCLASS.

CLASS lcl_object_ttyp DEFINITION INHERITING FROM lcl_objects_super FINAL.
  PUBLIC SECTION.
    INTERFACES lif_object.
ENDCLASS.
CLASS lcl_object_ttyp IMPLEMENTATION.
  METHOD lif_object~find_dependencies.
    DATA: ls_dd40l TYPE dd40l,
          lv_type  TYPE tadir-object.
    SELECT SINGLE * FROM dd40l INTO ls_dd40l WHERE typename = ms_item-obj_name.
    IF NOT ls_dd40l-rowtype IS INITIAL.
      lv_type = lcl_utils=>resolve( ls_dd40l-rowtype ).
      APPEND VALUE #( type  = lv_type name  = ls_dd40l-rowtype ) TO rt_result.
    ENDIF.
  ENDMETHOD.
ENDCLASS.

CLASS lcl_object_fugr DEFINITION INHERITING FROM lcl_objects_super FINAL.
  PUBLIC SECTION.
    INTERFACES lif_object.
ENDCLASS.
CLASS lcl_object_fugr IMPLEMENTATION.
  METHOD lif_object~find_dependencies.
* todo
  ENDMETHOD.
ENDCLASS.

CLASS lcl_object_xslt DEFINITION INHERITING FROM lcl_objects_super FINAL.
  PUBLIC SECTION.
    INTERFACES lif_object.
ENDCLASS.
CLASS lcl_object_xslt IMPLEMENTATION.
  METHOD lif_object~find_dependencies.
* todo
  ENDMETHOD.
ENDCLASS.

CLASS lcl_object_type DEFINITION INHERITING FROM lcl_objects_super FINAL.
  PUBLIC SECTION.
    INTERFACES lif_object.
ENDCLASS.
CLASS lcl_object_type IMPLEMENTATION.
  METHOD lif_object~find_dependencies.
* todo
  ENDMETHOD.
ENDCLASS.

CLASS lcl_object_intf DEFINITION INHERITING FROM lcl_objects_super FINAL.
  PUBLIC SECTION.
    INTERFACES lif_object.
ENDCLASS.
CLASS lcl_object_intf IMPLEMENTATION.
  METHOD lif_object~find_dependencies.
* todo
  ENDMETHOD.
ENDCLASS.

CLASS lcl_object_clas DEFINITION INHERITING FROM lcl_objects_super FINAL.
  PUBLIC SECTION.
    INTERFACES lif_object.
ENDCLASS.
CLASS lcl_object_clas IMPLEMENTATION.
  METHOD lif_object~find_dependencies.
* todo
  ENDMETHOD.
ENDCLASS.

CLASS lcl_object_doma DEFINITION INHERITING FROM lcl_objects_super FINAL.
  PUBLIC SECTION.
    INTERFACES lif_object.
ENDCLASS.

CLASS lcl_object_doma IMPLEMENTATION.
  METHOD lif_object~find_dependencies.
    RETURN. " no dependencies
  ENDMETHOD.
ENDCLASS.

CLASS lcl_object_tabl DEFINITION INHERITING FROM lcl_objects_super FINAL.
  PUBLIC SECTION.
    INTERFACES lif_object.
ENDCLASS.
CLASS lcl_object_tabl IMPLEMENTATION.
  METHOD lif_object~find_dependencies.
    DATA: lt_dd03l TYPE STANDARD TABLE OF dd03l WITH DEFAULT KEY.
    SELECT * FROM dd03l INTO TABLE lt_dd03l WHERE tabname = ms_item-obj_name.
    LOOP AT lt_dd03l ASSIGNING FIELD-SYMBOL(<ls_dd03l>).
      IF <ls_dd03l>-fieldname = '.INCLUDE'.
        APPEND VALUE #( type  = 'TABL' name  = <ls_dd03l>-precfield ) TO rt_result.
      ELSEIF NOT <ls_dd03l>-rollname IS INITIAL.
        APPEND VALUE #( type  = 'DTEL' name  = <ls_dd03l>-rollname ) TO rt_result.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.
ENDCLASS.

CLASS lcl_object_dtel DEFINITION INHERITING FROM lcl_objects_super FINAL.
  PUBLIC SECTION.
    INTERFACES lif_object.
ENDCLASS.

CLASS lcl_object_dtel IMPLEMENTATION.
  METHOD lif_object~find_dependencies.
    DATA: lv_domname TYPE dd04l-domname.
    SELECT SINGLE domname FROM dd04l INTO lv_domname WHERE rollname = ms_item-obj_name.
    IF sy-subrc = 0 AND NOT lv_domname IS INITIAL.
      APPEND VALUE #( type  = 'DOMA'  name = lv_domname ) TO rt_result.
    ENDIF.
  ENDMETHOD.
ENDCLASS.

CLASS lcl_object_prog DEFINITION INHERITING FROM lcl_objects_super FINAL.
  PUBLIC SECTION.
    INTERFACES lif_object.
ENDCLASS.

CLASS lcl_object_prog IMPLEMENTATION.
  METHOD lif_object~find_dependencies.

    DATA: lv_type      TYPE tadir-object,
          lv_foo       TYPE string,
          lt_cross     TYPE STANDARD TABLE OF cross WITH DEFAULT KEY,
          lt_wbcrossgt TYPE STANDARD TABLE OF wbcrossgt WITH DEFAULT KEY,
          lt_wbcrossi  TYPE STANDARD TABLE OF wbcrossi WITH DEFAULT KEY.


    SELECT * FROM wbcrossi INTO TABLE lt_wbcrossi
      WHERE include = ms_item-obj_name.
    LOOP AT lt_wbcrossi ASSIGNING FIELD-SYMBOL(<ls_wbcrossi>).
      CASE <ls_wbcrossi>-otype.
        WHEN 'IC'.
          lv_type = 'PROG'.
        WHEN OTHERS.
          BREAK-POINT.
      ENDCASE.
      APPEND VALUE #( type = lv_type name  = <ls_wbcrossi>-name ) TO rt_result.
    ENDLOOP.

    SELECT * FROM cross INTO TABLE lt_cross
      WHERE include = ms_item-obj_name
      AND name <> '?'.
    LOOP AT lt_cross ASSIGNING FIELD-SYMBOL(<ls_cross>).
      CASE <ls_cross>-type.
        WHEN 'F'.
          lv_type = 'FUGR'.
          <ls_cross>-name = lcl_utils=>resolve_func( <ls_cross>-name ).
          IF <ls_cross>-name IS INITIAL.
            CONTINUE. " seems to happen for ENQUEUE function modules
          ENDIF.
        WHEN '2'.
          lv_type = 'XSLT'.
        WHEN 'R'.
          lv_type = 'PROG'.
        WHEN 'G'.
          lv_type = 'TYPE'.
        WHEN 'N' OR 'P'.
          CONTINUE. " hmm, dunno
        WHEN 'U'.
          CONTINUE. " FORMs in programs
        WHEN OTHERS.
          BREAK-POINT.
      ENDCASE.
      APPEND VALUE #( type  = lv_type name  = <ls_cross>-name ) TO rt_result.
    ENDLOOP.

    SELECT * FROM wbcrossgt INTO TABLE lt_wbcrossgt
      WHERE include = ms_item-obj_name
      AND direct = abap_true.
    LOOP AT lt_wbcrossgt ASSIGNING FIELD-SYMBOL(<ls_wbcrossgt>).
      CLEAR lv_type.
      CASE <ls_wbcrossgt>-otype.
        WHEN 'DA'.
          IF <ls_wbcrossgt>-name = 'SY'
              OR <ls_wbcrossgt>-name CP 'SY\*'
              OR <ls_wbcrossgt>-name = 'ABAP_TRUE'
              OR <ls_wbcrossgt>-name = 'ABAP_FALSE'.
            CONTINUE.
          ENDIF.
          IF <ls_wbcrossgt>-name CP '*\ME:*'.
            lv_type = 'CLAS'.
            SPLIT <ls_wbcrossgt>-name AT '\ME:' INTO <ls_wbcrossgt>-name lv_foo.
          ENDIF.
        WHEN 'TY'.
          IF <ls_wbcrossgt>-name = 'ABAP_BOOL'
              OR <ls_wbcrossgt>-name = 'SYST'
              OR <ls_wbcrossgt>-name = 'ABAP_CHAR1'
              OR <ls_wbcrossgt>-name = 'SY'
              OR <ls_wbcrossgt>-name CP 'SY\*'
              OR <ls_wbcrossgt>-name CP 'SYST\*'.
            CONTINUE.
          ENDIF.
        WHEN 'ME'.
          lv_type = 'CLAS'.
          SPLIT <ls_wbcrossgt>-name AT '\ME:' INTO <ls_wbcrossgt>-name lv_foo.
        WHEN 'EV'.
          lv_type = 'CLAS'.
          SPLIT <ls_wbcrossgt>-name AT '\EV:' INTO <ls_wbcrossgt>-name lv_foo.
        WHEN OTHERS.
          BREAK-POINT.
      ENDCASE.

      IF <ls_wbcrossgt>-name CP '*\*'.
        SPLIT <ls_wbcrossgt>-name AT '\' INTO <ls_wbcrossgt>-name lv_foo.
      ENDIF.

      IF lv_type IS INITIAL.
        IF <ls_wbcrossgt>-name CP 'SEOC_*'
            OR <ls_wbcrossgt>-name CP 'SEOX_*'
            OR <ls_wbcrossgt>-name CP 'WDYN_*'
            OR <ls_wbcrossgt>-name CP 'ABAP_*'
            OR <ls_wbcrossgt>-name CP 'SEOK_*'
            OR <ls_wbcrossgt>-name CP 'CNHT_*'
            OR <ls_wbcrossgt>-name CP 'CNTL_*'
            OR <ls_wbcrossgt>-name CP 'TPAK_*'
            OR <ls_wbcrossgt>-name CP 'SKWFC_*'
            OR <ls_wbcrossgt>-name CP 'WBMR_*'
            OR <ls_wbcrossgt>-name CP 'STSTC_*'
            OR <ls_wbcrossgt>-name CP 'SWBM_*'
            OR <ls_wbcrossgt>-name CP 'SVRS2_*'
            OR <ls_wbcrossgt>-name CP 'ICON_*'
            OR <ls_wbcrossgt>-name CP 'TRSEL_*'
            OR <ls_wbcrossgt>-name CP 'TRWBO_*'
            OR <ls_wbcrossgt>-name CP 'SEWS_*'
            OR <ls_wbcrossgt>-name CP 'SEOP_*'.
* todo: how to handle type pools?
          CONTINUE.
        ENDIF.

        lv_type = lcl_utils=>resolve( <ls_wbcrossgt>-name ).
      ENDIF.

      APPEND VALUE #( type  = lv_type name  = <ls_wbcrossgt>-name ) TO rt_result.
    ENDLOOP.
  ENDMETHOD.
ENDCLASS.

***************************

CLASS lcl_dep DEFINITION.

  PUBLIC SECTION.
    CLASS-METHODS:
      analyze
        IMPORTING
          iv_type TYPE tadir-object
          iv_name TYPE tadir-obj_name
        RETURNING
          VALUE(rt_result) TYPE ty_result_tt.

  PRIVATE SECTION.
    CLASS-METHODS:
      find_dependencies
        IMPORTING
          is_result TYPE ty_result
        RETURNING
          VALUE(rt_result) TYPE ty_result_tt.

ENDCLASS.

CLASS lcl_dep IMPLEMENTATION.

  METHOD analyze.

    DATA: lv_index TYPE i.

    APPEND INITIAL LINE TO rt_result ASSIGNING FIELD-SYMBOL(<ls_res>).
    <ls_res>-type  = iv_type.
    <ls_res>-name  = iv_name.
    <ls_res>-level = 0.

    LOOP AT rt_result ASSIGNING <ls_res>.
      lv_index = sy-tabix + 1.
      INSERT LINES OF find_dependencies( <ls_res> ) INTO rt_result INDEX lv_index.
    ENDLOOP.

  ENDMETHOD.

  METHOD find_dependencies.

    DATA: lv_class  TYPE string,
          ls_item   TYPE ty_item,
          li_object TYPE REF TO lif_object.


    ls_item-obj_type = is_result-type.
    ls_item-obj_name = is_result-name.

    CONCATENATE 'LCL_OBJECT_' is_result-type INTO lv_class.

    TRY.
        CREATE OBJECT li_object TYPE (lv_class)
          EXPORTING
            is_item = ls_item.
      CATCH cx_sy_create_object_error.
* todo, missing implementation
        BREAK-POINT.
    ENDTRY.

    rt_result = li_object->find_dependencies( ).

    LOOP AT rt_result ASSIGNING FIELD-SYMBOL(<ls_result>).
      <ls_result>-level = is_result-level + 1.
    ENDLOOP.

    SORT rt_result BY type ASCENDING name ASCENDING.
    DELETE ADJACENT DUPLICATES FROM rt_result COMPARING type name.

  ENDMETHOD.

ENDCLASS.

************************************

START-OF-SELECTION.
  PERFORM run.

FORM run.

  DATA: lv_spaces TYPE string.

  DATA(lt_result) = lcl_dep=>analyze( iv_type = p_type
                                      iv_name = p_name ).

  LOOP AT lt_result ASSIGNING FIELD-SYMBOL(<ls_res>).
    CLEAR lv_spaces.
    DO <ls_res>-level TIMES.
      CONCATENATE space space lv_spaces INTO lv_spaces RESPECTING BLANKS.
    ENDDO.
    WRITE: / lv_spaces, <ls_res>-type, <ls_res>-name.
  ENDLOOP.

  WRITE: /.
  WRITE: / lines( lt_result ), 'results'.

  SORT lt_result BY type ASCENDING name ASCENDING.
  DELETE ADJACENT DUPLICATES FROM lt_result COMPARING type name.
  WRITE: / lines( lt_result ), 'unique results'.

ENDFORM.
