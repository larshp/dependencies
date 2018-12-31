REPORT zdependencies2.

START-OF-SELECTION.
  PERFORM run.

FORM run RAISING zcx_abapgit_exception.

  DATA: lv_obj_type       TYPE euobj-id,
        lt_environment    TYPE STANDARD TABLE OF senvi WITH DEFAULT KEY,
        lt_total          TYPE STANDARD TABLE OF senvi WITH DEFAULT KEY,
        lt_source_objects TYPE STANDARD TABLE OF rsfind WITH DEFAULT KEY.


  DATA(lt_objects) = zcl_abapgit_factory=>get_tadir( )->read( iv_package = '$ABAPGIT' ).
  DELETE lt_objects WHERE object = 'DEVC'.

  LOOP AT lt_objects INTO DATA(ls_object).
    lv_obj_type = ls_object-object.
    CALL FUNCTION 'REPOSITORY_ENVIRONMENT_SET'
      EXPORTING
        obj_type       = lv_obj_type
        object_name    = ls_object-obj_name
      TABLES
        environment    = lt_environment
      EXCEPTIONS
        batch          = 1
        batchjob_error = 2
        not_executed   = 3
        OTHERS         = 4.
    IF sy-subrc = 3.
      RETURN.
    ELSEIF sy-subrc <> 0.
      BREAK-POINT.
    ENDIF.
    APPEND LINES OF lt_environment TO lt_total.
  ENDLOOP.

  SORT lt_total BY type ASCENDING object ASCENDING.
  DELETE ADJACENT DUPLICATES FROM lt_total COMPARING type object.

  LOOP AT lt_total ASSIGNING FIELD-SYMBOL(<ls_total>).
    SELECT SINGLE devclass FROM tadir INTO <ls_total>-devclass
      WHERE pgmid = 'R3TR'
      AND object = <ls_total>-type
      AND obj_name = <ls_total>-object.
  ENDLOOP.
  DELETE lt_total WHERE devclass CP '$ABAPGIT*'.

  DELETE lt_total WHERE type = 'DGT' AND encl_obj = 'ABAP'.
  DELETE lt_total WHERE type = 'TYPE' AND object = 'ABAP'.
  DELETE lt_total WHERE type = 'OM'.
  DELETE lt_total WHERE type = 'OE'.
  DELETE lt_total WHERE type = 'OA'.
  DELETE lt_total WHERE type = 'OT'.
  DELETE lt_total WHERE type = 'DGT'.
  DELETE lt_total WHERE type = 'INCL'.

  LOOP AT lt_total INTO DATA(ls_total).
    WRITE: / ls_total-type, ls_total-object(40).
  ENDLOOP.

ENDFORM.
