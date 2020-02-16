report z_seo_class_output .

parameters:
  cifkey   type seoclskey,
  print(1).

type-pools:
  seoc,
  seop.

class cl_oo_include_naming definition load.

data:
  clstype     type seoclstype,
  source      type seop_source_string,
  pool_source type seop_source_string,
  source_line type line of seop_source_string,
  tabix       type sytabix,
  includes    type seop_methods_w_include,
  include     type seop_method_w_include,
  cifref      type ref to if_oo_clif_incl_naming,
  clsref      type ref to if_oo_class_incl_naming,
  intref      type ref to if_oo_interface_incl_naming,
  clipboard   type standard table of char256.

data: l_string type string.

start-of-selection.

  if print = 'X'.
    new-page print on.
  else.
    new-page.
  endif.
  call method cl_oo_include_naming=>get_instance_by_cifkey
    exporting
      cifkey = cifkey
    receiving
      cifref = cifref
    exceptions
      others = 1.
  if sy-subrc <> 0.
    message e003(oo) with cifkey.
  endif.

  case cifref->clstype.
    when seoc_clstype_class.
      clsref ?= cifref.
      read report clsref->class_pool
        into pool_source.
      loop at pool_source into source_line.
        if source_line cs 'CLASS-POOL'
          or source_line cs 'class-pool'.
          write / source_line.
          append source_line to clipboard.
          tabix = sy-tabix.
          exit.
        endif.
      endloop.
      skip.
      read report clsref->locals_old
        into source.
      loop at source
        into source_line.
        if source_line ns '*"*'.
          write / source_line.
          append source_line to clipboard.
        endif.
      endloop.
      if sy-subrc = 0.
        skip.
      endif.
      read report clsref->locals_def
        into source.
      loop at source
        into source_line.
        if source_line ns '*"*'.
          write / source_line.
          append source_line to clipboard.
        endif.
      endloop.
      if sy-subrc = 0.
        skip.
      endif.
      read report clsref->locals_imp
        into source.
      loop at source
        into source_line.
        if source_line ns '*"*'.
          write / source_line.
          append source_line to clipboard.
        endif.
      endloop.
      if sy-subrc = 0.
        skip.
      endif.
      read report clsref->macros
        into source.
      loop at source
        into source_line.
        if source_line ns '*"*'.
          write / source_line.
          append source_line to clipboard.
        endif.
      endloop.
      if sy-subrc = 0.
        skip.
      endif.
      read report clsref->public_section
        into source.
      loop at source
        into source_line.
        if source_line ns '*"*'.
          write / source_line.
          append source_line to clipboard.
        endif.
      endloop.
      skip.
      read report clsref->protected_section
        into source.
      loop at source
        into source_line.
        if source_line ns '*"*'.
          write / source_line.
          append source_line to clipboard.
        endif.
      endloop.
      skip.
      read report clsref->private_section
        into source.
      loop at source
        into source_line.
        if source_line ns '*"*'.
          write / source_line.
          append source_line to clipboard.
        endif.
      endloop.
      concatenate 'CLASS' cifkey 'IMPLEMENTATION' into l_string separated by space.
      loop at pool_source
        from tabix
        into source_line.
        if source_line cs 'ENDCLASS'.
          write / source_line.
          append source_line to clipboard.
        endif.
        if source_line cs l_string.
          skip.
          write / source_line.
          append source_line to clipboard.
          tabix = sy-tabix.
          exit.
        endif.
      endloop.
* method implementation
      includes = clsref->get_all_method_includes( ).
      loop at includes
        into include.
        read report include-incname
          into source.
        skip.
        loop at source
          into source_line.
          write / source_line.
          append source_line to clipboard.
        endloop.
      endloop.
      loop at pool_source
        from tabix
        into source_line.
        if source_line cs 'ENDCLASS'.
          write / source_line.
          append source_line to clipboard.
        endif.
      endloop.


    when seoc_clstype_interface.
      intref ?= cifref.
      read report intref->interface_pool
        into source.
      loop at source into source_line.
        write / source_line.
        append source_line to clipboard.
      endloop.
      skip.
      read report intref->public_section
        into source.
      loop at source into source_line.
        write / source_line.
        append source_line to clipboard.
      endloop.
      skip.

  endcase.

  check clipboard is not initial.

  data rc            type i.

  call method cl_gui_frontend_services=>clipboard_export
    importing
      data                 = clipboard
    changing
      rc                   = rc
    exceptions
      cntl_error           = 1
      error_no_gui         = 2
      not_supported_by_gui = 3
      no_authority         = 4
      others               = 5.
  if sy-subrc <> 0.
* Implement suitable error handling here
  elseif rc = 0.
    message 'Source copied to clipboard' type 'S'.
  endif.
