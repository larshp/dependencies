REPORT zdependencies2.

PARAMETERS: p_devc TYPE devclass OBLIGATORY,
            p_down TYPE c AS CHECKBOX DEFAULT 'X',
            p_path TYPE text200,
            p_max  TYPE i DEFAULT 2.

DATA: gt_total TYPE if_ris_environment_types=>ty_t_senvi_tadir.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_path.
  DATA: gv_sel TYPE string.
  cl_gui_frontend_services=>directory_browse(
    CHANGING
      selected_folder = gv_sel ).
  p_path = gv_sel.

START-OF-SELECTION.
  PERFORM run.

FORM get_dependencies
    USING is_object TYPE zif_abapgit_definitions=>ty_tadir
          iv_level TYPE i
    CHANGING ct_tadir TYPE if_ris_environment_types=>ty_t_senvi_tadir.

  CLEAR ct_tadir.
  IF iv_level > p_max.
    RETURN.
  ENDIF.

  DATA: lv_obj_type    TYPE euobj-id,
        lt_environment TYPE senvi_tab.

  lv_obj_type = is_object-object.
  CALL FUNCTION 'REPOSITORY_ENVIRONMENT_SET'
    EXPORTING
      obj_type       = lv_obj_type
      object_name    = is_object-obj_name
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

  cl_wb_ris_environment=>convert_senvi_to_tadir(
    EXPORTING
      senvi = lt_environment
    IMPORTING
      senvi_tadir = ct_tadir ).

  DATA(lt_result) = ct_tadir.
  DATA: lt_tadir LIKE lt_result.
  DATA(lv_level) = iv_level + 1.
  LOOP AT ct_tadir INTO DATA(ls_tadir).
    DATA(ls_object) = VALUE zif_abapgit_definitions=>ty_tadir(
      object = ls_tadir-ref_obj_type
      obj_name = ls_tadir-ref_obj_name ).

    PERFORM get_dependencies USING ls_object lv_level CHANGING lt_tadir.
    APPEND LINES OF lt_tadir TO lt_result.
  ENDLOOP.

  SORT lt_result BY ref_obj_type ASCENDING ref_obj_name ASCENDING.
  DELETE ADJACENT DUPLICATES FROM lt_result COMPARING ref_obj_type ref_obj_name.

  ct_tadir = lt_result.

ENDFORM.

FORM run RAISING zcx_abapgit_exception.

  DATA: lt_tadir TYPE if_ris_environment_types=>ty_t_senvi_tadir.


  DATA(lt_objects) = zcl_abapgit_factory=>get_tadir( )->read( p_devc ).
  DELETE lt_objects WHERE object = 'DEVC'.

  LOOP AT lt_objects INTO DATA(ls_object).
    PERFORM get_dependencies USING ls_object 1 CHANGING lt_tadir.
    APPEND LINES OF lt_tadir TO gt_total.
  ENDLOOP.

  SORT gt_total BY ref_obj_type ASCENDING ref_obj_name ASCENDING.
  DELETE ADJACENT DUPLICATES FROM gt_total COMPARING ref_obj_type ref_obj_name.

  LOOP AT gt_total ASSIGNING FIELD-SYMBOL(<ls_total>).
    DATA(lv_index) = sy-tabix.
    SELECT SINGLE devclass FROM tadir INTO @DATA(lv_devclass)
      WHERE pgmid = 'R3TR'
      AND object = @<ls_total>-ref_obj_type
      AND obj_name = @<ls_total>-ref_obj_name.
* todo, should check if its a subclass
    IF sy-subrc <> 0 OR lv_devclass CP |{ p_devc }*|.
      DELETE gt_total INDEX lv_index.
    ENDIF.
  ENDLOOP.

  DELETE gt_total WHERE ref_obj_type = 'TYPE'
    OR ref_obj_type = 'PROG'
    OR ref_obj_type = 'FUGR'.

  LOOP AT gt_total INTO DATA(ls_total).
    WRITE: / ls_total-ref_obj_type, ls_total-ref_obj_name.
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

  lv_folder = p_path.
  IF lv_folder IS INITIAL.
    RETURN.
  ENDIF.

  LOOP AT gt_total INTO DATA(ls_total).
    DATA(ls_files_item) = zcl_abapgit_objects=>serialize(
      is_item = VALUE #( obj_type = ls_total-ref_obj_type obj_name = ls_total-ref_obj_name )
      iv_language = sy-langu ).

    PERFORM build_clas CHANGING ls_files_item.

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

FORM build_clas CHANGING files TYPE zcl_abapgit_objects=>ty_serialization.

  IF files-item-obj_type <> 'CLAS'.
    RETURN.
  ENDIF.

* todo, this is everything?
  DELETE files-files WHERE filename CP '*.clas.locals_def.abap'.
  DELETE files-files WHERE filename CP '*.clas.locals_imp.abap'.
  DELETE files-files WHERE filename CP '*.clas.macros.abap'.
  DELETE files-files WHERE filename CP '*.clas.xml'. " todo?
  DELETE files-files WHERE filename CP '*.clas.testclasses.abap'.

  DATA: lt_text   TYPE abaptxt255_tab,
        lv_string TYPE string.

  PERFORM build_code USING files-item-obj_name CHANGING lt_text.
  CONCATENATE LINES OF lt_text INTO lv_string SEPARATED BY cl_abap_char_utilities=>newline.

  LOOP AT files-files ASSIGNING FIELD-SYMBOL(<ls_file>) WHERE filename CP '*.clas.abap'.
    <ls_file>-data = zcl_abapgit_convert=>string_to_xstring_utf8( lv_string ).
  ENDLOOP.

ENDFORM.

FORM build_code USING class TYPE clike CHANGING ct_code TYPE abaptxt255_tab RAISING cx_class_not_existent.

  DATA lt_text TYPE abaptxt255_tab.

  DATA(lo_class) = CAST cl_oo_class( cl_oo_class=>get_instance( CONV #( class ) ) ).
  DATA(lt_includes) = cl_oo_classname_service=>get_all_method_includes( CONV #( class ) ).
  DATA(lt_methods) = lo_class->get_methods( ).
  DATA(lv_final) = lo_class->is_final( ).

  DATA(lv_include) = cl_oo_classname_service=>get_pubsec_name( CONV #( class ) ).
  READ REPORT lv_include INTO lt_text.
  APPEND LINES OF lt_text TO ct_code.

  IF lv_final = abap_false.
    lv_include = cl_oo_classname_service=>get_prosec_name( CONV #( class ) ).
    READ REPORT lv_include INTO lt_text.
    APPEND LINES OF lt_text TO ct_code.
  ENDIF.

  APPEND |ENDCLASS.| TO ct_code.
  APPEND |CLASS { to_lower( class ) } IMPLEMENTATION.| TO ct_code.

  LOOP AT lt_includes INTO DATA(ls_include).
    IF line_exists( lt_methods[ cmpname = ls_include-cpdkey-cpdname exposure = 0 ] ).
      CONTINUE. " private method
    ELSEIF lv_final = abap_true AND line_exists( lt_methods[ cmpname = ls_include-cpdkey-cpdname exposure = 1 ] ).
      CONTINUE. " protected method
    ENDIF.
    READ REPORT ls_include-incname INTO lt_text.
    IF lines( lt_text ) = 1 AND lt_text[ 1 ] = '*** inactive new ***'.
      CONTINUE.
    ENDIF.
    APPEND |  METHOD { to_lower( ls_include-cpdkey-cpdname ) }.| TO ct_code.
    APPEND |  ENDMETHOD.| TO ct_code.
  ENDLOOP.

  APPEND |ENDCLASS.| TO ct_code.

  PERFORM pretty_print CHANGING ct_code.

ENDFORM.

FORM pretty_print CHANGING ct_code TYPE abaptxt255_tab.

  CALL FUNCTION 'PRETTY_PRINTER'
    EXPORTING
      inctoo = abap_true
    TABLES
      ntext  = ct_code
      otext  = ct_code.

ENDFORM.
