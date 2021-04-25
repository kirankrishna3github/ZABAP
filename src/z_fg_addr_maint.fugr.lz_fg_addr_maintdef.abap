class lcl_app definition final.
  public section.
    class-data mv_address type adrc-addrnumber.

    class-methods:
      restrict_sel_opt,
      sel_screen_pbo,
      sel_screen_pai,
      download_file_format.

    methods process.

  private section.
endclass.

class lcl_main definition final.
  public section.
    class-methods: start.
endclass.
