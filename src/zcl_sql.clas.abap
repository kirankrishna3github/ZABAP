class ZCL_SQL definition
  public
  final
  create public .

public section.

  types:
    begin of GTY_MESSAGES,
      message type bapi_msg,
    end of GTY_MESSAGES .
  types:
    GTTY_MESSAGES type standard table of GTY_MESSAGES .

  methods CONSTRUCTOR .
  methods MAIN .
  class-methods CREATE_DDIC_OBJECT
    importing
      value(IV_CON_NAME) type DBCON-CON_NAME
      value(IV_DB_TABNAME) type ADBC_NAME
      value(IV_SCHEMA) type ADBC_SCHEMA_NAME
      value(IV_OBJ_TYPE) type TADIR-OBJECT
      value(IV_OBJ_NAME) type DDOBJNAME optional
      value(IV_PKG) type DEVCLASS optional
    exporting
      value(ET_MESSAGES) type GTTY_MESSAGES
    returning
      value(RV_OK) type BOOLEAN .
protected section.
private section.

  data GO_SQL_STMT type ref to CL_SQL_STATEMENT .
  data GV_CON_NAME type DBCON-CON_NAME .
  data GO_CON type ref to CL_SQL_CONNECTION .
  data GV_SQL_STMT type STRING .

  methods DECODE_AND_EXECUTE .
  methods GET_STATEMENT_REF .
  methods CLOSE_CONNECTION .
  methods GET_CONNECTION .
  methods GET_CONNECTION_PARAMS .
  methods GET_SQL_STATEMENT .
  methods DISPLAY_MESSAGE
    importing
      value(IV_MESSAGE) type BAPI_MSG .
ENDCLASS.



CLASS ZCL_SQL IMPLEMENTATION.


  method close_connection.
    try.
        if not go_con->is_closed( ).
          go_con->close( ).
          free go_con.
          display_message( exporting iv_message = |Connection { gv_con_name } closed successfully. Exiting...| ).
          leave program.
        endif.
      catch cx_sql_exception into data(lox_sql).
        display_message( exporting iv_message = |Error: Connection Close: { lox_sql->get_text( ) }| ).
    endtry.
  endmethod.


  method CONSTRUCTOR.
  endmethod.


  method create_ddic_object.
    " 0. This method supports creation of ddic structure(iv_obj_type = STRU) and table(iv_obj_type = TABL) using sql result set metadata
    " 1. Connect to the database using the dbcon supplied as input
    " 2. Execute select single on the given table on the connected db
    " 3. Use the result set to retrieve table metadata from the connected db
    " 4. Check if a consistent abap struct can be generated, only then proceed for ddic generation
    " 5. Check object name already exists in ddic
    " 6. Generate ddic compatible metadata using result set metadata
    " 7. Create the table
    " 8. RS_CORR_INSERT: Assign it to a package(create tadir entry) and insert it into correction request in case of transportable package
    " 9. Check the generated object for syntax errors
    " 10. Activate the object or delete it if there are syntax errors

    " local data
    data: lv_obj_name type tabname,
          ls_dd02v    type dd02v, " table header
          ls_dd09l    type dd09v, " technical settings => for table; not applicable for structure
          lt_dd03p    type standard table of dd03p, " fields
          ls_dd03p    type dd03p,
          lv_pkg      type devclass.

    " local constants
    constants: lc_english          type ddlanguage   value 'E',       " English
               lc_cl_transp_tab    type tabclass     value 'TRANSP',  " transparent table
               lc_cl_struct        type tabclass     value 'INTTAB',  " structure
               lc_table            type tadir-object value 'TABL',    " object type table
               lc_struct           type tadir-object value 'STRU',    " object type structure
               lc_delv_cl_a        type contflag     value 'A',       " Application table (master and transaction data)
               lc_enh_cat_nc       type ddranking    value '1',       " Cannot be enhanced
               lc_data_cl_appl1    type tabart       value 'APPL1',   " Transactional data
               lc_tab_cat_0        type tabkat       value '0',       " Size category
               lc_dtype_dec        type datatype_d   value 'DEC',
               lc_dtype_float      type datatype_d   value 'FLTP',
               lc_dtype_int1       type datatype_d   value 'INT1',
               lc_dtype_int2       type datatype_d   value 'INT2',
               lc_dtype_int4       type datatype_d   value 'INT4',
               lc_dtype_int8       type datatype_d   value 'INT8',
               lc_dtype_char       type datatype_d   value 'CHAR',
               lc_dtype_raw        type datatype_d   value 'RAW',
               lc_dtype_string     type datatype_d   value 'SSTR',
               lc_dtype_rawstring  type datatype_d   value 'RSTR',
               lc_dtype_decfloat16 type datatype_d   value 'D16D',
               lc_dtype_decfloat34 type datatype_d   value 'D34D',
               lc_mandt            type rollname     value 'MANDT',
               lc_r3tr             type pgmid        value 'R3TR',    " PGMID: R3TR
               lc_temp_pkg         type devclass     value '$TMP',    " Default local package
               lc_dd_objtyp_tab    type ddeutype     value 'T',       " Short object type form for table/structure
               lc_corr_obj_cl_dict type c length 4   value 'DICT',    " Object class for DDIC objects in RS_CORR_INSERT
               lc_corr_mode_insert type c length 1   value 'I',       " Insert mode for RS_CORR_INSERT
               lc_objtyp_tabdef    type tadir-object value 'TABD',    " Object type table definition
               lc_objtyp_tech      type tadir-object value 'TABT'.    " Object type technical settings

    try.
        rv_ok = abap_false. refresh et_messages.

        " only table and structure creation is supported
        if iv_obj_type ne lc_table and iv_obj_type ne lc_struct.
          append value #( message = |Object type { iv_obj_type } not supported| ) to et_messages.
          return.
        endif.

        " Default object name is ZTABLENAME
        clear: lv_obj_name.
        if iv_obj_name is supplied and iv_obj_name is not initial.
          if iv_obj_name+0(1) ne 'Y' and iv_obj_name+0(1) ne 'Z'. " supplied name should be in customer namespace
            append value #( message = |Object name { iv_obj_name } not in customer name space| ) to et_messages.
            return.
          else.
            lv_obj_name = iv_obj_name.
          endif.
        else.
          lv_obj_name = `Z` && to_upper( iv_db_tabname ).
        endif.

        " Default package is $TMP => local objects
        clear lv_pkg.
        if iv_pkg is supplied and iv_pkg is not initial.
          lv_pkg = iv_pkg.
        else.
          lv_pkg = lc_temp_pkg.
        endif.

        " Create connection to the specified DBCON
        data(lo_con) = cl_sql_connection=>get_connection( exporting con_name = iv_con_name ).

        if lo_con is bound.
          " some pre-checks and data gatherring
          data(lo_meta) = lo_con->get_metadata( ).
          if lo_meta is bound.
            lo_meta->get_columns(
              exporting
                schema_name      = iv_schema
                table_name       = iv_db_tabname
              importing
                column_tab       = data(lt_column) ).
          endif.

          if lt_column is not initial.
            " Execute a select single query on the given table in remote db
            data(lv_sql_stmt) = `select * from ` && to_upper( val = iv_schema ) && `.` && to_upper( val = iv_db_tabname )
                                && ` where rownum = 1` .
            data(lo_sql_stmt) = lo_con->create_statement( ).
            if lo_sql_stmt is bound.
              data(lo_res_set) = lo_sql_stmt->execute_query( exporting statement = lv_sql_stmt ).
              if lo_res_set is bound.
                " Generate structure metadata of the remote table using the result set
                data(lt_metadata) = lo_res_set->get_metadata( ).
                if lt_metadata is not initial.
                  data(lr_struct) = lo_res_set->get_struct_ref( md_tab = lt_metadata ).
                  if lr_struct is bound.
                    lo_res_set->close( ).
                    lo_con->close( ).
                    data(lo_struct) = cast cl_abap_structdescr( cl_abap_typedescr=>describe_by_data_ref( lr_struct ) ).
                    zcl_sql_wms=>format_metadata( exporting it_col_metadata = lt_column changing it_metadata = lt_metadata ).
                    if lo_struct is bound.  " Ensures a consistent abap type creation
                      clear: ls_dd02v, ls_dd09l, ls_dd03p.
                      refresh lt_dd03p.

                      select single * from dd02l into @data(ls_dd02l) where tabname = @lv_obj_name.
                      if sy-subrc <> 0. " object already exists?

                        " populate object header
                        ls_dd02v-tabname    = lv_obj_name.
                        ls_dd02v-ddlanguage = lc_english.
                        ls_dd02v-ddtext     = `Generated ` &&
                                              cond #( when iv_obj_type = lc_table then `table`
                                                      when iv_obj_type = lc_struct then `structure`
                                                      else `non-supported object type` ) && " this will never happen
                                              ` for: ` && to_upper( iv_db_tabname ).
                        case iv_obj_type.
                          when lc_table.
                            ls_dd02v-tabclass   = lc_cl_transp_tab.
                            ls_dd02v-clidep     = abap_true.
                            ls_dd02v-contflag   = lc_delv_cl_a.
                          when lc_struct.
                            ls_dd02v-tabclass   = lc_cl_struct.
                          when others.
                        endcase.
                        ls_dd02v-exclass = lc_enh_cat_nc.

                        if iv_obj_type eq lc_table.
                          " populate technical settings, only in case of table
                          ls_dd09l-tabname = lv_obj_name.
                          ls_dd09l-tabkat  = lc_tab_cat_0.
                          ls_dd09l-tabart  = lc_data_cl_appl1.
                        endif.

                        " populate table fields
                        data: lv_pos type i.
                        " insert a client field in case of table
                        if iv_obj_type eq lc_table.
                          lv_pos = lv_pos + 1.
                          clear ls_dd03p.
                          ls_dd03p-tabname    = lv_obj_name.
                          ls_dd03p-fieldname  = lc_mandt.
                          ls_dd03p-ddlanguage = lc_english.
                          ls_dd03p-rollname = lc_mandt.
                          ls_dd03p-position = lv_pos.
                          ls_dd03p-keyflag = abap_true.
                          ls_dd03p-notnull = abap_true.
                          append ls_dd03p to lt_dd03p.
                        endif.

                        " Map result set metadata types to ababp ddic types
                        clear ls_dd03p.
                        loop at lt_metadata into data(ls_metadata).
                          lv_pos = lv_pos + 1.
                          ls_dd03p-tabname    = lv_obj_name.
                          ls_dd03p-fieldname  = ls_metadata-column_name.
                          ls_dd03p-ddlanguage = lc_english.
                          ls_dd03p-ddtext     = ls_dd03p-fieldname.
*                        try.
*                            data(ls_component) = lo_struct->components[ name = ls_dd03p-fieldname ].
*                          catch cx_sy_itab_line_not_found.
*                        endtry.
                          ls_dd03p-position = lv_pos.
                          ls_dd03p-decimals = ls_metadata-decimals.
                          ls_dd03p-leng     = ls_metadata-length.

                          case ls_metadata-data_type.
                            when cl_abap_typedescr=>typekind_packed.
                              ls_dd03p-datatype = lc_dtype_dec.
                            when cl_abap_typedescr=>typekind_float.
                              ls_dd03p-datatype = lc_dtype_float.
                            when cl_abap_typedescr=>typekind_int.
                              if ls_metadata-length <= 1.
                                ls_dd03p-datatype = lc_dtype_int1.
                              elseif ls_metadata-length <= 2.
                                ls_dd03p-datatype = lc_dtype_int2.
                              elseif ls_metadata-length <= 4.
                                ls_dd03p-datatype = lc_dtype_int4.
                              else.
                                ls_dd03p-datatype = lc_dtype_int8.
                              endif.
                            when cl_abap_typedescr=>typekind_char.
                              ls_dd03p-datatype = lc_dtype_char.
                            when cl_abap_typedescr=>typekind_hex.
                              ls_dd03p-datatype = lc_dtype_raw.
                            when cl_abap_typedescr=>typekind_string.
                              ls_dd03p-datatype = lc_dtype_string.
                            when cl_abap_typedescr=>typekind_xstring.
                              ls_dd03p-datatype = lc_dtype_rawstring.
                            when cl_abap_typedescr=>typekind_decfloat16.
                              ls_dd03p-datatype = lc_dtype_decfloat16.
                            when cl_abap_typedescr=>typekind_decfloat34.
                              ls_dd03p-datatype = lc_dtype_decfloat34.
                            when others.
                          endcase.

                          append ls_dd03p to lt_dd03p.
                          clear: ls_metadata, ls_dd03p.
                        endloop.

                        if lt_dd03p is not initial.
                          " Create the table in DDIC
                          call function 'DDIF_TABL_PUT'
                            exporting
                              name              = conv ddobjname( lv_obj_name )
                              dd02v_wa          = ls_dd02v
                              dd09l_wa          = ls_dd09l
                            tables
                              dd03p_tab         = lt_dd03p
                            exceptions
                              tabl_not_found    = 1
                              name_inconsistent = 2
                              tabl_inconsistent = 3
                              put_failure       = 4
                              put_refused       = 5
                              others            = 6.
                          if sy-subrc = 0.  " add object to package and TR(in case of transportable package)
                            data: ls_ddenq         type ddenqs,
                                  lv_corr_pkg      type tadir-devclass,
                                  lv_corr_korrnum  type e070-trkorr,
                                  lv_corr_ordernum type e070-trkorr,
                                  lv_corr_new      type c length 1,
                                  lv_corr_author   type sy-uname,
                                  ls_corr_trkey    type trkey.

                            clear: ls_ddenq, lv_corr_pkg, lv_corr_korrnum,
                                   lv_corr_ordernum, lv_corr_new, lv_corr_author, ls_corr_trkey.

                            " add main table
                            ls_ddenq-objname = lv_obj_name.
                            ls_ddenq-objtype = lc_table.

                            " this is a multi-pupose FM,
                            " creates a TR and TADIR entry(package assignment) for transportable packages;
                            " creates only TADIR entry(package assignment) for local packages;
                            " prompts from TR creation/selection, returns the generated TR
                            call function 'RS_CORR_INSERT'
                              exporting
                                object              = ls_ddenq
                                object_class        = lc_corr_obj_cl_dict
                                mode                = lc_corr_mode_insert
                                global_lock         = abap_true
                                devclass            = conv tadir-devclass( lv_pkg )
                                author              = sy-uname
                                master_language     = lc_english
                                suppress_dialog     = abap_false
                                mod_langu           = lc_english
                              importing
                                devclass            = lv_corr_pkg
                                korrnum             = lv_corr_korrnum
                                ordernum            = lv_corr_ordernum
                                new_corr_entry      = lv_corr_new
                                author              = lv_corr_author
                                transport_key       = ls_corr_trkey
                              exceptions
                                cancelled           = 1
                                permission_failure  = 2
                                unknown_objectclass = 3
                                others              = 4.
                            if sy-subrc = 0.  " check and activate the object
                              " Add table definition to the TR, FM is smart enough to put the tab def in the same TR as the main table
                              ls_ddenq-objtype = lc_objtyp_tabdef.

                              call function 'RS_CORR_INSERT'
                                exporting
                                  object              = ls_ddenq
                                  object_class        = lc_corr_obj_cl_dict
                                exceptions
                                  cancelled           = 1
                                  permission_failure  = 2
                                  unknown_objectclass = 3
                                  others              = 4.

                              " Add table technical settings to the TR, FM is smart enough to put the tech settings in the same TR as the main table
                              if iv_obj_type eq lc_table.
                                ls_ddenq-objtype = lc_objtyp_tech.

                                call function 'RS_CORR_INSERT'
                                  exporting
                                    object              = ls_ddenq
                                    object_class        = lc_corr_obj_cl_dict
                                    suppress_dialog     = abap_false
                                  exceptions
                                    cancelled           = 1
                                    permission_failure  = 2
                                    unknown_objectclass = 3
                                    others              = 4.
                              endif.

                              data: lv_syn_check_objtype  type rsedd0-ddobjtype,
                                    lv_check_result       type sysubrc,
                                    lt_syn_check_messages type ddmessages.

                              clear: lv_syn_check_objtype, lv_check_result.
                              refresh: lt_syn_check_messages.

                              " SAP treats Table, Struct both as TABL
                              lv_syn_check_objtype = cond #( when iv_obj_type = lc_table then lc_dd_objtyp_tab
                                                             when iv_obj_type = lc_struct then lc_dd_objtyp_tab ).

                              " Syntax check
                              call function 'RS_DD_CHECK'
                                exporting
                                  objname        = conv rsedd0-ddobjname( lv_obj_name )
                                  objtype        = lv_syn_check_objtype
                                  with_messages  = abap_true
                                importing
                                  e_check_result = lv_check_result
                                  messages       = lt_syn_check_messages.

                              if lv_check_result eq 0.  " proceed for activation if syntax check is a success

                                data: lv_rc type i.
                                clear lv_rc.
                                call function 'DDIF_TABL_ACTIVATE'
                                  exporting
                                    name        = conv ddobjname( lv_obj_name )
                                    auth_chk    = abap_false
                                  importing
                                    rc          = lv_rc
                                  exceptions
                                    not_found   = 1
                                    put_failure = 2
                                    others      = 3.

                                if lv_rc = 0.
                                  rv_ok = abap_true.
                                else.
                                  append value #( message = |Error in object activation| ) to et_messages.
                                endif.
                              else.  " deleted the object if syntax check fails
                                data: lv_protname    type protname,
                                      lv_del_corrnum type e070-trkorr.

                                clear: lv_protname, lv_del_corrnum.
                                lv_del_corrnum = lv_corr_korrnum.
                                call function 'RS_DD_DELETE_OBJ'
                                  exporting
                                    no_ask               = abap_true
                                    objname              = conv rsedd0-ddobjname( lv_obj_name )
                                    objtype              = conv rsedd0-ddobjtype( lc_dd_objtyp_tab )
                                  importing
                                    protname             = lv_protname
                                  changing
                                    corrnum              = lv_del_corrnum
                                  exceptions
                                    not_executed         = 1
                                    object_not_found     = 2
                                    object_not_specified = 3
                                    permission_failure   = 4
                                    dialog_needed        = 5
                                    others               = 6.
                                if sy-subrc <> 0.
* Implement suitable error handling here
                                else.
                                  data(lv_deleted) = boolc( sy-subrc = 0 ).
                                endif.

                                " Add messages from syntax check to message tab
                                append value #( message = |Syntax error in generated object. See next message/s| ) to et_messages.

                                data(lt_syn_sy_messages) = cl_sbd_logging_utility=>wrap_messages_dd_to_sy(
                                                            exporting i_dd_messages = lt_syn_check_messages ).
                                loop at lt_syn_sy_messages into data(ls_syn_sy_message).
                                  append value #( message = conv bapi_msg( new cl_demo_message_texts(
                                                                                msgid = ls_syn_sy_message-msgid
                                                                                msgno = ls_syn_sy_message-msgno
                                                                                msgv1 = conv string( ls_syn_sy_message-msgv1 )
                                                                                msgv2 = conv string( ls_syn_sy_message-msgv2 )
                                                                                msgv3 = conv string( ls_syn_sy_message-msgv3 )
                                                                                msgv4 = conv string( ls_syn_sy_message-msgv4 ) )->get_text( ) ) )
                                                                                to et_messages.
                                  clear ls_syn_sy_message.
                                endloop.
                                if lv_deleted  = abap_true.
                                  append value #( message = |Object deleted| ) to et_messages.
                                endif.
                              endif.
                              " add other messages
                            else.
                              append value #( message = |Error in package assignment/transport creation| ) to et_messages.
                            endif.
                          else.
                            append value #( message = |Error in object creation| ) to et_messages.
                          endif.
                        else.
                          append value #( message = |Field table could not be generated| ) to et_messages.
                        endif.
                      else.
                        append value #( message = |DDIC Object { lv_obj_name } already exists| ) to et_messages.
                      endif.
                    else.
                      append value #( message = |Could not get struct descriptor| ) to et_messages.
                    endif.
                  else.
                    append value #( message = |Could not get result set struct ref| ) to et_messages.
                  endif.
                else.
                  append value #( message = |Could not fetch result metadata| ) to et_messages.
                endif.
              else.
                append value #( message = |Result set could not be generated| ) to et_messages.
              endif.
            else.
              append value #( message = |Statement object could not be created| ) to et_messages.
            endif.
          else.
            append value #( message = |Remote table does not exist| ) to et_messages.
          endif.
        else.
          append value #( message = |Connection to { iv_con_name } failed| ) to et_messages.
        endif.
      catch cx_sql_exception into data(lox_sql).
        append value #( message = conv bapi_msg( lox_sql->get_text( ) ) ) to et_messages.
      catch cx_root into data(lox).
        append value #( message = conv bapi_msg( lox->get_text( ) ) ) to et_messages.
    endtry.
  endmethod.


  method decode_and_execute.
    try.
        check go_con is bound and go_con->ping( ) and go_sql_stmt is bound and gv_sql_stmt is not initial.
        data: lv_query type xfeld,
              lv_ddl   type xfeld,
              lv_dml   type xfeld,
              lv_tcl   type xfeld.
        " determine statement type
        split gv_sql_stmt at space into table data(lt_stmt).
        check lt_stmt is not initial.
        clear: lv_query, lv_dml, lv_ddl, lv_tcl.
        case condense( to_upper( lt_stmt[ 1 ] ) ).
          when 'SELECT'.
            lv_query = abap_true.
          when 'INSERT'.
            lv_dml = abap_true.
          when 'UPDATE'.
            lv_dml = abap_true.
          when 'DELETE'.
            lv_dml = abap_true.
          when 'CREATE'.
            lv_ddl = abap_true.
          when 'ALTER'.
            lv_ddl = abap_true.
          when 'DROP'.
            lv_ddl = abap_true.
          when 'TRUNCATE_TABLE' or 'TRUNCATE'.
            lv_ddl = abap_true.
          when 'GRANT'.
            lv_ddl = abap_true.
          when 'REVOKE'.
            lv_ddl = abap_true.
          when others.
            display_message( exporting iv_message = |Error: Statement not supported| ).
            return.
        endcase.

        if lv_query eq abap_true.
          data(lo_res_set) = go_sql_stmt->execute_query( exporting statement = gv_sql_stmt ).
          if lo_res_set is bound.
            data(lt_metadata) = lo_res_set->get_metadata( ).
            zcl_sql_wms=>format_metadata( changing it_metadata = lt_metadata ).

            try.
                data: lo_type_descr   type ref to cl_abap_typedescr,
                      lo_struct_descr type ref to cl_abap_structdescr,
                      lo_table        type ref to data.
                free: lo_struct_descr, lo_type_descr, lo_table.

                data(lo_struct) = lo_res_set->get_struct_ref( exporting md_tab = lt_metadata ).

                call method cl_abap_typedescr=>describe_by_data_ref
                  exporting
                    p_data_ref           = lo_struct
                  receiving
                    p_descr_ref          = lo_type_descr
                  exceptions
                    reference_is_initial = 1
                    others               = 2.
                if sy-subrc <> 0.
*                 Implement suitable error handling here
                else.
                  lo_struct_descr ?= lo_type_descr.
                endif.

                data(lo_tabletype) = cl_abap_tabledescr=>create(
                                      exporting
                                        p_line_type = lo_struct_descr
                                        p_table_kind = cl_abap_tabledescr=>tablekind_std ).

                create data lo_table type handle lo_tabletype.
                field-symbols: <lt_data> type standard table.
                assign lo_table->* to <lt_data>.
                if <lt_data> is assigned.
                  refresh <lt_data>.
                endif.

                lo_res_set->set_param_table( exporting itab_ref = lo_table ).
                lo_res_set->next_package( ).
                lo_res_set->close( ).

                if <lt_data> is  assigned.
                  display_message( exporting iv_message = |{ lo_res_set->rows_fetched } rows fetched successfully| ).
                  if <lt_data> is not initial.
                    cl_demo_output=>display( <lt_data> ).
                  endif.
                endif.
              catch cx_sy_table_creation into data(lox_table_creation).
                display_message( exporting iv_message = conv bapi_msg( lox_table_creation->get_text( ) ) ).
              catch cx_sy_struct_creation into data(lox_struct_create).
                display_message( exporting iv_message = conv bapi_msg( lox_struct_create->get_text( ) ) ).
              catch cx_parameter_invalid_range into data(lox_parameter_invalid_range).
                display_message( exporting iv_message = conv bapi_msg( lox_parameter_invalid_range->get_text( ) ) ).
            endtry.
          endif.
        endif.

        if lv_dml eq abap_true.
          data(lv_rows_processed) = go_sql_stmt->execute_update( exporting statement = gv_sql_stmt ).
          if lv_rows_processed ge 1.
            go_con->commit( ).
            display_message( exporting iv_message = |{ lv_rows_processed } rows processed successfully| ).
          endif.
        endif.

        if lv_ddl eq abap_true.
          go_sql_stmt->execute_ddl( exporting statement = gv_sql_stmt ).
          display_message( exporting iv_message = |Statement executed successfully| ).
        endif.

      catch cx_sql_exception into data(lox_sql).
        try.
            go_con->rollback( ).
          catch cx_sql_exception.
        endtry.
        display_message( exporting iv_message = conv bapi_msg( lox_sql->get_text( ) ) ).
      catch cx_parameter_invalid into data(lox_parameter_invalid).
        try.
            go_con->rollback( ).
          catch cx_sql_exception.
        endtry.
        display_message( exporting iv_message = conv bapi_msg( lox_parameter_invalid->get_text( ) ) ).
    endtry.
  endmethod.


  method display_message.
    data: title     type spop-titel value 'Message',
          textline1 type spop-textline1,
          textline2 type spop-textline2,
          textline3 type spop-textline3.

    clear: textline1, textline2, textline3.
    textline1 = iv_message+0(70).
    if strlen( iv_message ) gt 70.
      textline2 = iv_message+70(70).
    endif.
    if strlen( iv_message ) gt 140.
      textline3 = iv_message+140(70).
    endif.
    call function 'POPUP_TO_DISPLAY_TEXT_LO'
      exporting
        titel        = title
        textline1    = textline1
        textline2    = textline2
        textline3    = textline3
        start_column = ( sy-scols / 2 ) - 40
        start_row    = ( sy-srows / 2 ) - 5.

  endmethod.


  method get_connection.
    check gv_con_name is not initial.
    try .
        free go_con.
        go_con = cl_sql_connection=>get_connection( exporting con_name = gv_con_name ).
        if go_con is bound and go_con->ping( ).
          display_message( exporting iv_message = |Connected to { gv_con_name } successfully| ).
        else.
          display_message( exporting iv_message = |Error: Connection to { gv_con_name } could not be established| ).
          leave program.
        endif.
      catch cx_sql_exception into data(lox_sql).
        display_message( exporting iv_message = |Error: Connection to { gv_con_name } could not be established: { lox_sql->get_text( ) }| ).
        leave program.
    endtry.
  endmethod.


  method get_connection_params.
    data: lv_retcode type c length 1.
    data(lt_fields) = value ty_sval( ( tabname = 'DBCON' fieldname = 'CON_NAME' field_obl = abap_true ) ).
    clear: lv_retcode, gv_con_name.
    call function 'POPUP_GET_VALUES'
      exporting
        popup_title     = |Select DB Connection|
        start_column    = ( sy-scols / 2 ) - 40
        start_row       = ( sy-srows / 2 ) - 5
      importing
        returncode      = lv_retcode
      tables
        fields          = lt_fields
      exceptions
        error_in_fields = 1
        others          = 2.
    if sy-subrc <> 0.
* Implement suitable error handling here
      display_message( exporting iv_message = |Error: Could not set connection params.| ).
      leave program.
    else.
      case lv_retcode.
        when space.
          gv_con_name = lt_fields[ 1 ]-value.
        when 'A'.
          display_message( exporting iv_message = |Error: Action cancelled by user.| ).
          leave program.
        when others.
      endcase.
    endif.
  endmethod.


  method get_sql_statement.
    data lt_text type catsxt_longtext_itab.

    refresh lt_text.
    clear: gv_sql_stmt.
    call function 'CATSXT_SIMPLE_TEXT_EDITOR'
      exporting
        im_title        = conv sytitle( |SQL Statement: Type 'exit' to leave| )
        im_start_row    = ( sy-srows / 2 ) - 5
        im_start_column = ( sy-scols / 2 ) - 40
      changing
        ch_text         = lt_text.

    if sy-ucomm eq 'CX_CONT'.
      if lt_text is not initial.
        delete lt_text where table_line is initial.
        if lt_text is not initial.
          loop at lt_text into data(ls_text).
            gv_sql_stmt = gv_sql_stmt && condense( ls_text ) && ` `.
            clear ls_text.
          endloop.
          if gv_sql_stmt is not initial.
            if to_lower( gv_sql_stmt+0(4) ) cs 'exit'.
              close_connection( ).
            endif.
          endif.
        endif.
      else.
        display_message( exporting iv_message = |Error: Empty SQL Statement| ).
      endif.
    else.
      display_message( exporting iv_message = |Error: Action cancelled by user. Type 'exit' to leave.| ).
    endif.

  endmethod.


  method get_statement_ref.
    check go_con is bound and go_con->ping( ) and gv_sql_stmt is not initial.
    free go_sql_stmt.
    go_sql_stmt = go_con->create_statement( ).
    if not go_sql_stmt is bound.
      display_message( exporting iv_message = |Error: Could not create statment object| ).
    endif.
  endmethod.


  method main.
    try.
        get_connection_params( ).
        check gv_con_name is not initial.
        get_connection( ).
        check go_con is bound and go_con->ping( ).
        display_message( exporting iv_message = |Type 'exit' in command box to leave| ).
        do.
          get_sql_statement( ).
          check gv_sql_stmt is not initial.
          get_statement_ref( ).
          check go_sql_stmt is bound.
          decode_and_execute( ).
        enddo.
        close_connection( ).
      catch cx_sql_exception into data(lox_sql).
        display_message( exporting iv_message = conv bapi_msg( lox_sql->get_text( ) ) ).
    endtry.

  endmethod.
ENDCLASS.
