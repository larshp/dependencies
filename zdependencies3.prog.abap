REPORT zdependencies3.

DATA: gt_total TYPE if_ris_environment_types=>ty_t_senvi_tadir.

PARAMETERS: p_devc TYPE devclass OBLIGATORY,
            p_mlvl TYPE i DEFAULT 30,
            p_max  TYPE i DEFAULT 9999.

PARAMETERS: p_down TYPE c AS CHECKBOX DEFAULT 'X',
            p_path TYPE text200.

START-OF-SELECTION.
  PERFORM run.
  IF p_down = abap_true.
    PERFORM download.
  ENDIF.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_path.
  DATA: gv_sel TYPE string.
  cl_gui_frontend_services=>directory_browse(
    CHANGING
      selected_folder = gv_sel ).
  p_path = gv_sel.

FORM run RAISING zcx_abapgit_exception.

  DATA: lt_tadir TYPE if_ris_environment_types=>ty_t_senvi_tadir.


  DATA(lt_objects) = zcl_abapgit_factory=>get_tadir( )->read( p_devc ).
  DELETE lt_objects WHERE object = 'DEVC'.
  DELETE lt_objects WHERE object = 'TRAN'.

* todo, skip generated maintenance view function groups?

  LOOP AT lt_objects INTO DATA(ls_object).
    IF sy-tabix > p_max.
      EXIT.
    ENDIF.

    cl_progress_indicator=>progress_indicate(
      i_text               = |Processing, { sy-tabix }|
      i_processed          = sy-tabix
      i_total              = lines( lt_objects )
      i_output_immediately = abap_true ).

    DATA(lt_parents) = VALUE zif_abapgit_definitions=>ty_tadir_tt( (
      object = ls_object-object
      obj_name = ls_object-obj_name ) ).
    PERFORM get_dependencies USING ls_object 1.
  ENDLOOP.

  SORT gt_total BY ref_obj_type ASCENDING ref_obj_name ASCENDING.
  DELETE ADJACENT DUPLICATES FROM gt_total COMPARING ref_obj_type ref_obj_name.

  LOOP AT gt_total INTO DATA(ls_total).
    WRITE: / ls_total-ref_obj_type, ls_total-ref_obj_name.
  ENDLOOP.

ENDFORM.

FORM find_clas_dependencies USING pv_name TYPE tadir-obj_name
                                  pv_level TYPE i
                         CHANGING ct_tadir TYPE if_ris_environment_types=>ty_t_senvi_tadir.

  DATA: lt_includes TYPE STANDARD TABLE OF programm WITH EMPTY KEY.

  TRY.
      DATA(lv_final) = CAST cl_oo_class( cl_oo_class=>get_instance( CONV #( pv_name ) ) )->is_final( ).
    CATCH cx_class_not_existent.
      RETURN.
  ENDTRY.

  APPEND cl_oo_classname_service=>get_pubsec_name( CONV #( pv_name ) ) TO lt_includes.
  IF lv_final = abap_false.
    APPEND cl_oo_classname_service=>get_prisec_name( CONV #( pv_name ) ) TO lt_includes.
  ENDIF.

  DATA: lt_wbcrossgt TYPE wbcrossgtt.
  SELECT * FROM wbcrossgt INTO CORRESPONDING FIELDS OF TABLE @lt_wbcrossgt
    FOR ALL ENTRIES IN @lt_includes
    WHERE include = @lt_includes-table_line
    AND name <> @pv_name.

  IF pv_level < p_mlvl.
    PERFORM resolve USING lt_wbcrossgt CHANGING ct_tadir.
  ELSE.
    BREAK-POINT.
  ENDIF.

ENDFORM.

FORM resolve USING it_wbcrossgt TYPE wbcrossgtt
          CHANGING ct_tadir TYPE if_ris_environment_types=>ty_t_senvi_tadir.

  LOOP AT it_wbcrossgt INTO DATA(ls_wbcrossgt).
    CASE ls_wbcrossgt-otype.
      WHEN 'TY'.
        SELECT SINGLE clstype FROM seoclass INTO @DATA(lv_clstype) WHERE clsname = @ls_wbcrossgt-name(30).
        IF sy-subrc = 0.
          CASE lv_clstype.
            WHEN '0'.
              APPEND VALUE #( ref_obj_type = 'CLAS' ref_obj_name = ls_wbcrossgt-name ) TO ct_tadir.
            WHEN '1'.
              APPEND VALUE #( ref_obj_type = 'INTF' ref_obj_name = ls_wbcrossgt-name ) TO ct_tadir.
            WHEN OTHERS.
              ASSERT 0 = 1.
          ENDCASE.
        ENDIF.
      WHEN OTHERS.
        CONTINUE. " todo ?
    ENDCASE.
  ENDLOOP.

ENDFORM.

FORM get_dependencies
    USING is_object TYPE zif_abapgit_definitions=>ty_tadir
    iv_level TYPE i.

  DATA: lv_obj_type    TYPE euobj-id,
        lt_tadir       TYPE if_ris_environment_types=>ty_t_senvi_tadir,
        lt_environment TYPE senvi_tab.

  IF is_object-obj_name = 'J_1AFITP'.
    BREAK-POINT.
  ENDIF.

  IF iv_level > 1 AND is_object-object = 'CLAS'.
    PERFORM find_clas_dependencies USING is_object-obj_name iv_level CHANGING lt_tadir.
  ELSEIF is_object-object = 'TABL'.
* do not traverse further
  ELSE.
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
        senvi       = lt_environment
      IMPORTING
        senvi_tadir = lt_tadir ).
  ENDIF.

  SORT lt_tadir BY ref_obj_type ASCENDING ref_obj_name ASCENDING.
  DELETE ADJACENT DUPLICATES FROM lt_tadir COMPARING ref_obj_type ref_obj_name.

  DELETE lt_tadir WHERE ref_obj_type = 'FUGR'.
  DELETE lt_tadir WHERE ref_obj_type = 'DTEL'.
  DELETE lt_tadir WHERE ref_obj_type = 'SFSW'.
  DELETE lt_tadir WHERE ref_obj_type = 'DEVC'.
*  DELETE lt_tadir WHERE ref_obj_type = 'TABL'.
  DELETE lt_tadir WHERE ref_obj_type = 'SUSO'.
  DELETE lt_tadir WHERE ref_obj_type = 'VIEW'.
  DELETE lt_tadir WHERE ref_obj_type = 'TYPE'.
  DELETE lt_tadir WHERE ref_obj_type = 'TTYP'.
  DELETE lt_tadir WHERE ref_obj_type = 'PROG'.
  DELETE lt_tadir WHERE ref_obj_type = 'DOMA'.
  DELETE lt_tadir WHERE ref_obj_type = 'XSLT'.
  DELETE lt_tadir WHERE ref_obj_type = 'SHLP'.
  DELETE lt_tadir WHERE ref_obj_type = 'SQLT'.

  LOOP AT lt_tadir INTO DATA(ls_tadir).
    DATA(lv_index) = sy-tabix.
    SELECT SINGLE devclass FROM tadir INTO @DATA(lv_devclass)
      WHERE pgmid = 'R3TR'
      AND object = @ls_tadir-ref_obj_type
      AND obj_name = @ls_tadir-ref_obj_name.
* todo, should check if its a subclass
    IF sy-subrc <> 0 OR lv_devclass CP |{ p_devc }*|.
      DELETE lt_tadir INDEX lv_index.
      CONTINUE.
    ENDIF.

    READ TABLE gt_total WITH KEY ref_obj_type = ls_tadir-ref_obj_type
      ref_obj_name = ls_tadir-ref_obj_name TRANSPORTING NO FIELDS.
    IF sy-subrc = 0.
      DELETE lt_tadir INDEX lv_index.
    ENDIF.
  ENDLOOP.

  APPEND LINES OF lt_tadir TO gt_total.

  LOOP AT lt_tadir INTO ls_tadir.
    DATA(ls_object) = VALUE zif_abapgit_definitions=>ty_tadir(
      object = ls_tadir-ref_obj_type
      obj_name = ls_tadir-ref_obj_name ).
    DATA(lv_level) = iv_level + 1.
    PERFORM get_dependencies USING ls_object lv_level.
  ENDLOOP.

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
    cl_progress_indicator=>progress_indicate(
      i_text               = |Downloading|
      i_processed          = sy-tabix
      i_total              = lines( gt_total )
      i_output_immediately = abap_true ).

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
          show_transfer_status = abap_false
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
    ELSEIF NOT line_exists( lt_methods[ cmpname = ls_include-cpdkey-cpdname ] ).
      CONTINUE.
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
