*&---------------------------------------------------------------------*
*& Report Z_PRG_ADJUST_VARIANTS
*&---------------------------------------------------------------------*
*& << S/4HANA / 6010859 / SBXK900310 / Wednesday, November 21, 2018 16:06:29
*&---------------------------------------------------------------------*
*& Post upgrade issue: Variant is out of sync with the current state of the program sel screen
*& SAP has provided std program: RSVARDOC_610 for adjustments
*& Problem: RSVARDOC_610 when scheduled for mass processing exits with various runtime errors(mostly abap access denied and type mismatch)
*& Solution: This program submits RSVARDOC_610 via bg job separately for each program to be adjusted
*& This allows the execution to proceed even if one of the jobs fails
*& Log is generated based on the status of the job, since RSVAROC_610 does not generate any log
*&---------------------------------------------------------------------*
report z_prg_adjust_variants.

* -- global data -- *
data: file type rlgrap-filename,
      prog type rsvar-report.

* -- selection screen -- *
selection-screen begin of block opt with frame title text-opt.
parameters: r_auto radiobutton group rad user-command cmd default 'X',
            r_file radiobutton group rad.
selection-screen end of block opt.
selection-screen begin of block sel with frame title text-sel.
select-options: s_repid for prog modif id rep.
selection-screen comment /1(70) rep modif id rep.
parameters: p_file like file modif id fil.
selection-screen comment /1(70) fil modif id fil.
selection-screen end of block sel.

* -- local class definition -- *
class lcl_application definition final.
  public section.
    class-methods: open_file, screen_modif.

    methods: process.

  protected section.

  private section.
    type-pools: icon.

    methods: clear_data, read_file, get_programs_to_be_adjusted, adjust_variants, display_log.

    data: begin of data,
            report type rsvarkey-report,
          end of data,
          data_tab like standard table of data,

          begin of log,
            report   type rsvar-report,
            jobname  type tbtcjob-jobname,
            jobcount type tbtcjob-jobcount,
            status   type icon_d,
            message  type bapi_msg,
          end of log,
          log_tab like standard table of log.
endclass.

class main definition.
  public section.
    class-methods: start.

  protected section.

  private section.
endclass.

* -- local class implementation -- *
class lcl_application implementation.
  method open_file.
    call function 'F4_FILENAME'
      exporting
        field_name = 'P_FILE'
      importing
        file_name  = p_file.
  endmethod.

  method screen_modif.
    loop at screen.
      case abap_true.
        when r_auto.
          if screen-group1 eq 'FIL'.
            screen-active = 0.
          endif.
          if screen-group1 eq 'REP'.
            screen-active = 1.
          endif.
          modify screen.
        when r_file.
          if screen-group1 eq 'FIL'.
            screen-active = 1.
          endif.
          if screen-group1 eq 'REP'.
            screen-active = 0.
          endif.
          modify screen.
        when others.
      endcase.
    endloop.
  endmethod.

  method process.
    clear_data( ).  " clear all private, public and global data if required
    case abap_true.
      when r_auto.  " auto-mode, uses rsvcheck to get programs to be adjusted
        get_programs_to_be_adjusted( ). " submit rsvcheck and extract list of programs to be adjusted from memory
      when r_file.  " file upload, previously(manually) generated list of programs to be adjusted
        if p_file is not initial.
          read_file( ). " convert xl data to itab
        endif.
      when others.
    endcase.
    if data_tab is not initial.
      adjust_variants( ). " submit 1 job of RSVARDOC_610 for each of the programs to be adjusted
    else.
      write 'No data read/No adjustments required'.
    endif.
    if log_tab is not initial.
      display_log( ). " alv log output
    else.
      write / 'No log generated'.
    endif.
  endmethod.

  method clear_data.
    refresh: data_tab, log_tab.
    clear: log, data.
  endmethod.

  method read_file.
    if p_file is not initial.
      "convert xls to sap itab
      data i_tab_raw_data type truxs_t_text_data.
      refresh data_tab.
      call function 'TEXT_CONVERT_XLS_TO_SAP'
        exporting
          i_field_seperator    = abap_true
          i_line_header        = abap_true
          i_tab_raw_data       = i_tab_raw_data
          i_filename           = p_file
        tables
          i_tab_converted_data = data_tab
        exceptions
          conversion_failed    = 1
          others               = 2.
      if sy-subrc <> 0.
* Implement suitable error handling here
      endif.
    endif.
  endmethod.

  method get_programs_to_be_adjusted.
    submit rsvcheck with report in s_repid exporting list to memory and return.
    call function 'SAPGUI_PROGRESS_INDICATOR'
      exporting
        percentage = 50
        text       = 'Fetching programs/variants to be adjusted...'.

    data: list_tab type standard table of abaplist.
    refresh list_tab.
    call function 'LIST_FROM_MEMORY'
      tables
        listobject = list_tab
      exceptions
        not_found  = 1
        others     = 2.

    check list_tab is not initial.
    data: list_asci type soli_tab.
    refresh list_asci.
    call function 'LIST_TO_ASCI'
      tables
        listasci           = list_asci
        listobject         = list_tab
      exceptions
        empty_list         = 1
        list_index_invalid = 2
        others             = 3.
    if sy-subrc <> 0.
* Implement suitable error handling here
    endif.

    call function 'LIST_FREE_MEMORY'
      tables
        listobject = list_tab.

    check list_asci is not initial.

    " delete extraneous lines and extract program names
    data(lo_regex) = new cl_abap_regex( pattern = '\S+' ignore_case = abap_true ).
    delete list_asci where ( line+0(1) na sy-abcde and line+0(1) ne '/' ).
    " lines beginning with special chars and numerals cannot be program names(except '/')
    loop at list_asci into data(list_asci_wa).
      clear data.
      find all occurrences of regex lo_regex in list_asci_wa-line results data(lt_words).
      if sy-subrc = 0.
        " lines with more than 2 words{ prog_name variant_name } are extra lines/metadata/headers
        if lines( lt_words ) gt 2.  " debug and you will see why
          delete list_asci.
        else.
          data-report = condense( to_upper( list_asci_wa-line+0(40) ) ).  " first 40 chars is always report name since rsvar-report length = 40
          append data to data_tab.
        endif.
      endif.
      refresh: lt_words.
      clear: list_asci_wa.
    endloop.

    check data_tab is not initial.
    delete data_tab where report is initial.
    sort data_tab by report.
    delete adjacent duplicates from data_tab comparing report. " since each prog is repeated multiple times for each variant
    " we will process prog directly rather than in prog-variant pair to...
    " ...save on time as well as job numbers(though that would have given us a more detailed log)
  endmethod.

  method adjust_variants.
    data: l_jobcount  type tbtcjob-jobcount,
          l_jobname   type tbtcjob-jobname,
          aborted     type tbtcv-abort,
          finished    type tbtcv-fin,
          preliminary type tbtcv-prelim,
          ready       type tbtcv-ready,
          running     type tbtcv-run,
          scheduled   type tbtcv-sched,
          suspended   type btcstatus,
          other       type btcstatus,
          joblogtbl   type standard table of tbtc5.

    if data_tab is not initial.
      clear data.
      loop at data_tab into data.
        clear: l_jobcount, l_jobname, log.
        data-report = condense( to_upper( data-report ) ).
        log-report = data-report.
        l_jobname = |{ data-report }_AV|. " jobname = reportname_av => for easy identification

        call function 'JOB_OPEN'  " create a job
          exporting
            jobname          = l_jobname
          importing
            jobcount         = l_jobcount
          exceptions
            cant_create_job  = 1
            invalid_job_data = 2
            jobname_missing  = 3
            others           = 4.
        if sy-subrc <> 0.
          log-status = icon_red_light.
          message id sy-msgid type sy-msgty number sy-msgno into log-message
          with sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
        else.

          submit rsvardoc_610 with s_report-low = data-report " submit report via job created above
                user sy-uname via job l_jobname
                number l_jobcount and return.

          call function 'JOB_CLOSE' " release the job
            exporting
              jobcount             = l_jobcount
              jobname              = l_jobname
              strtimmed            = 'X'
            exceptions
              cant_start_immediate = 1
              invalid_startdate    = 2
              jobname_missing      = 3
              job_close_failed     = 4
              job_nosteps          = 5
              job_notex            = 6
              lock_failed          = 7
              others               = 8.
          if sy-subrc <> 0.
            log-status = icon_red_light.
            message id sy-msgid type sy-msgty number sy-msgno into log-message
            with sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
          endif.
        endif.
        log-jobname = l_jobname.
        log-jobcount = l_jobcount.

        " check for job completion status in loop at 2 second intervals
        " stop checking if job is finished/aborted or exceeds 10 minutes and proceed to next program
        get time stamp field data(before_while).
        clear: aborted, finished, preliminary, ready, running, scheduled, suspended, other.
        while not aborted eq abap_true and not finished eq abap_true.
          message 'Adjusting variants for program' && ` ` && data-report type 'S'.
          call function 'SHOW_JOBSTATE'
            exporting
              jobcount         = l_jobcount
              jobname          = l_jobname
            importing
              aborted          = aborted
              finished         = finished
              preliminary      = preliminary
              ready            = ready
              running          = running
              scheduled        = scheduled
              suspended        = suspended
              other            = other
            exceptions
              jobcount_missing = 1
              jobname_missing  = 2
              job_notex        = 3
              others           = 4.
          if sy-subrc <> 0.
* Implement suitable error handling here
          endif.
          get time stamp field data(in_while).
          if ( in_while - before_while ) >= 600
              or ( aborted eq abap_true or finished eq abap_true ). " stuck for more that 10 minutes...continue to next program
            exit.
          endif.
          clear in_while.
          wait up to 1 seconds.
        endwhile.

        " update log
        case abap_true.
          when aborted.
            log-status  = icon_red_light.
            log-message = 'Aborted'.

            refresh joblogtbl.

            call function 'BP_JOBLOG_READ'  " get failure reason
              exporting
                jobcount              = l_jobcount
                jobname               = l_jobname
              tables
                joblogtbl             = joblogtbl
              exceptions
                cant_read_joblog      = 1
                jobcount_missing      = 2
                joblog_does_not_exist = 3
                joblog_is_empty       = 4
                joblog_name_missing   = 5
                jobname_missing       = 6
                job_does_not_exist    = 7
                others                = 8.
            if sy-subrc <> 0.
* Implement suitable error handling here
            else.
              read table joblogtbl into data(joblog) index 3.
              if sy-subrc = 0.
                log-message = log-message && ` ` && '-' && ` ` && joblog-text.
              endif.
            endif.
          when finished.
            log-status  = icon_green_light.
            log-message = 'Finished'.
          when preliminary or ready or running or scheduled or suspended or other.
            log-status  = icon_yellow_light.
            log-message = 'Time-out - Check SM37'.
          when others.
        endcase.
        append log to log_tab.
        clear: data, before_while, in_while.
      endloop.
    endif.
  endmethod.

  method display_log.
    data: lo_table     type ref to cl_salv_table,
          lo_functions type ref to cl_salv_functions_list,
          lo_columns   type ref to cl_salv_columns_table,
          lo_column    type ref to cl_salv_column_table,
          lo_display   type ref to cl_salv_display_settings.

    check log_tab is not initial.
    try.
        free: lo_table, lo_functions, lo_columns, lo_display.
        call method cl_salv_table=>factory
          importing
            r_salv_table = lo_table
          changing
            t_table      = log_tab.

        check lo_table is bound.
        lo_columns = lo_table->get_columns( ).

        if lo_columns is bound.
          try.
              free lo_column.
              lo_column ?= lo_columns->get_column( columnname = 'STATUS' ).
              lo_column->set_long_text( value = 'Processing Status' ).
              lo_column->set_medium_text( value = 'Proc Stat' ).
              lo_column->set_short_text( value = 'ProcStat' ).
            catch cx_salv_not_found.                    "#EC NO_HANDLER
              lo_columns->set_optimize( exporting value = if_salv_c_bool_sap=>true ). " Default input bool true
          endtry.
        endif.

        lo_display = lo_table->get_display_settings( ).

        if lo_display is bound.
          lo_display->set_striped_pattern( exporting value = cl_salv_display_settings=>true ).
          lo_display->set_list_header( exporting value = 'Adjustment Log' ).
        endif.

        lo_functions = lo_table->get_functions( ).

        if lo_functions is bound.
          lo_functions->set_all( exporting value = if_salv_c_bool_sap=>true ). " Default input bool true
        endif.

        lo_table->display( ).
      catch cx_salv_msg.                                "#EC NO_HANDLER
    endtry.
  endmethod.
endclass.

class main implementation.
  method start.
    data(lo_app) = new lcl_application( ).

    check lo_app is bound.
    lo_app->process( ).
  endmethod.
endclass.

* -- initialisation -- *
initialization.
  rep = 'Please specify the report names or run the program in background'.
  fil = 'Excel file must contain single column with header. Eg: Report'.

* -- selection screen events -- *
at selection-screen on value-request for p_file.
  lcl_application=>open_file( ).

at selection-screen output.
  lcl_application=>screen_modif( ).

* -- start of selection/main -- *
start-of-selection.
  main=>start( ).
