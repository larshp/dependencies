REPORT zdependencies2.

PARAMETERS: p_devc TYPE devclass DEFAULT '$ABAPGIT',
            p_down TYPE c AS CHECKBOX DEFAULT 'X'.

DATA: gt_total TYPE STANDARD TABLE OF senvi WITH DEFAULT KEY.

START-OF-SELECTION.
  PERFORM run.

FORM run RAISING zcx_abapgit_exception.

  DATA: lv_obj_type       TYPE euobj-id,
        lt_environment    TYPE STANDARD TABLE OF senvi WITH DEFAULT KEY,
        lt_source_objects TYPE STANDARD TABLE OF rsfind WITH DEFAULT KEY.


  DATA(lt_objects) = zcl_abapgit_factory=>get_tadir( )->read( iv_package = p_devc ).
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
    APPEND LINES OF lt_environment TO gt_total.
  ENDLOOP.

  SORT gt_total BY type ASCENDING object ASCENDING.
  DELETE ADJACENT DUPLICATES FROM gt_total COMPARING type object.

  LOOP AT gt_total ASSIGNING FIELD-SYMBOL(<ls_total>).
    SELECT SINGLE devclass FROM tadir INTO <ls_total>-devclass
      WHERE pgmid = 'R3TR'
      AND object = <ls_total>-type
      AND obj_name = <ls_total>-object.
  ENDLOOP.
  DELETE gt_total WHERE devclass CP |{ p_devc }*|.

  DELETE gt_total WHERE type = 'DGT' AND encl_obj = 'ABAP'.
  DELETE gt_total WHERE type = 'TYPE' AND object = 'ABAP'.
  DELETE gt_total WHERE type = 'OM'.
  DELETE gt_total WHERE type = 'OE'.
  DELETE gt_total WHERE type = 'OA'.
  DELETE gt_total WHERE type = 'OT'.
  DELETE gt_total WHERE type = 'DGT'.
  DELETE gt_total WHERE type = 'INCL'.

  LOOP AT gt_total INTO DATA(ls_total).
    WRITE: / ls_total-type, ls_total-object(40).
  ENDLOOP.

  IF p_down = abap_true.
    PERFORM download.
  ENDIF.

ENDFORM.

FORM download RAISING zcx_abapgit_exception.

  DATA: lv_folder   TYPE string,
        lv_fullpath TYPE string,
        lt_rawdata  TYPE solix_tab,
        lv_sep      TYPE c LENGTH 1.


  cl_gui_frontend_services=>get_file_separator(
    CHANGING
      file_separator = lv_sep ).

  cl_gui_frontend_services=>directory_browse(
    CHANGING
      selected_folder = lv_folder ).
  IF lv_folder IS INITIAL.
    RETURN.
  ENDIF.

  LOOP AT gt_total INTO DATA(ls_total) WHERE type = 'CLAS' OR type = 'INTF'.
    DATA(ls_files_item) = zcl_abapgit_objects=>serialize(
      is_item = VALUE #(  obj_type = ls_total-type obj_name = ls_total-object )
      iv_language = sy-langu ).

    LOOP AT ls_files_item-files ASSIGNING FIELD-SYMBOL(<ls_file>).
      CONCATENATE lv_folder lv_sep <ls_file>-filename INTO lv_fullpath.

      lt_rawdata = cl_bcs_convert=>xstring_to_solix( <ls_file>-data ).

      cl_gui_frontend_services=>gui_download(
        EXPORTING
          bin_filesize              = xstrlen( <ls_file>-data )
          filename                  = lv_fullpath
          filetype                  = 'BIN'
        CHANGING
          data_tab                  = lt_rawdata
        EXCEPTIONS
          file_write_error          = 1
          no_batch                  = 2
          gui_refuse_filetransfer   = 3
          invalid_type              = 4
          no_authority              = 5
          unknown_error             = 6
          header_not_allowed        = 7
          separator_not_allowed     = 8
          filesize_not_allowed      = 9
          header_too_long           = 10
          dp_error_create           = 11
          dp_error_send             = 12
          dp_error_write            = 13
          unknown_dp_error          = 14
          access_denied             = 15
          dp_out_of_memory          = 16
          disk_full                 = 17
          dp_timeout                = 18
          file_not_found            = 19
          dataprovider_exception    = 20
          control_flush_error       = 21
          not_supported_by_gui      = 22
          error_no_gui              = 23
          OTHERS                    = 24 ).
      IF sy-subrc <> 0.
        BREAK-POINT.
        RETURN.
      ENDIF.
    ENDLOOP.

  ENDLOOP.

ENDFORM.
