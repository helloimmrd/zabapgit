*&---------------------------------------------------------------------*
*& Report ZSRIMM_R0001
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT YSRILOP_R00012D.

INCLUDE YSRILOP_R00012D_TOP.
INCLUDE YSRILOP_R00012D_F01.

START-OF-SELECTION.

  PERFORM f_get_data.

 END-OF-SELECTION.


 IF NOT it_data[] IS INITIAL.
    PERFORM f_display_data.
  ELSE.
    MESSAGE 'Data Not Found' TYPE 'S' DISPLAY LIKE 'E'.
  ENDIF.
