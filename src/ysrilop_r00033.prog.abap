*&---------------------------------------------------------------------*
*& Report YSRILOP_R00033
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT ysrilop_r00033.

INCLUDE : ysrilop_r00033_top,
          ysrilop_r00033_f01.

START-OF-SELECTION.

  PERFORM f_get_data.

  IF p_report EQ 'X'.
    PERFORM report_excel.
  ENDIF.

END-OF-SELECTION.

  IF NOT it_data[] IS INITIAL.
    PERFORM build_f_catalog.
    PERFORM f_display_data.
  ELSE.
    MESSAGE 'Data Not Found' TYPE 'S' DISPLAY LIKE 'E'.
  ENDIF.
