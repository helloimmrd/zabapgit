*&---------------------------------------------------------------------*
*& Include          ZSRIMM_R0001_F01
*&---------------------------------------------------------------------*
FORM f_get_data.

  DATA: itline  TYPE i,
        wa_line TYPE i,
        lv_con  TYPE c LENGTH 25.

*  TYPES: BEGIN OF ty_agr51,
*           low TYPE agr_1251-low,
*         END OF ty_agr51.

  TYPES: BEGIN OF ty_agr52,
           low TYPE agr_1251-low,

         END OF ty_agr52.

  DATA: it_agr51 TYPE STANDARD TABLE OF agr_1251,
        wa_agr51 TYPE agr_1251,
        it_agr52 TYPE STANDARD TABLE OF agr_1252,
        wa_agr52 TYPE agr_1252.

  lv_con = |{ 'ZP:SRI_' }{ sy-uname }|.
*  lv_con = |{ 'ZP:SRI_' }{ 'ITE-ST301' }|.

*    Filter Purch. Group
  SELECT * INTO TABLE @it_agr51 FROM agr_1251 WHERE agr_name = @lv_con
    AND object = 'M_BANF_EKG'
    AND field = 'EKGRP'.

  IF sy-subrc = 0.

    LOOP AT it_agr51 INTO wa_agr51.

      IF wa_agr51-low NE '$EKGRP'.

        IF listbox = '2' OR listbox = 'X' OR listbox = ''.
          SELECT
            a~ernam, a~loekz, a~ekgrp, a~purreqndescription ,a~banfn, a~bnfpo, a~badat,a~matnr, a~matkl, a~txz01, a~bsmng, a~meins, a~preis, a~lfdat, b~ps_psp_pnr, b~kostl,
            c~ebeln, c~ebelp, a~frgkz, a~frgzu, c~aedat, e~eindt, e~wemng, d~lifnr, g~name1, c~netpr, d~waers, d~wkurs, c~mwskz, c~netwr, f~belnr,
            f~buzei, f~budat, f~bwart, f~menge, f~dmbtr, f~wrbtr, f~gjahr
        INTO TABLE @it_ret
        FROM
            eban AS a
            LEFT JOIN
            ebkn AS b
            ON a~banfn = b~banfn
            AND a~bnfpo = b~bnfpo
            LEFT JOIN
            ekpo AS c
            ON a~banfn = c~banfn
            AND a~bnfpo = c~bnfpo
            AND c~loekz = ''
            LEFT JOIN
            ekko AS d
            ON c~ebeln = d~ebeln
            LEFT JOIN
            eket AS e
            ON c~ebeln = e~ebeln
            AND c~ebelp = e~ebelp
            LEFT JOIN
            ekbe AS f
            ON c~ebeln = f~ebeln
            AND c~ebelp = f~ebelp
            AND f~bewtp = 'E'
            LEFT JOIN
            lfa1 AS g
            ON d~lifnr = g~lifnr
            WHERE
              a~banfn IN @s_prnum
              AND a~badat IN @s_badat
              AND a~matnr IN @s_matnr
              AND b~ps_psp_pnr IN @s_wbs
              AND a~frgkz EQ @listbox
              AND a~ekgrp EQ @wa_agr51-low
        ORDER BY
            a~banfn,
            a~bnfpo.
          IF sy-subrc = 0.

            LOOP AT it_ret INTO DATA(wa_result).
*              INSERT wa_result INTO it_ret2 INDEX wa_line.
              APPEND wa_result TO it_ret2.
            ENDLOOP.

          ENDIF.
        ELSEIF listbox = '1'.
          SELECT
          a~ernam, a~loekz, a~ekgrp, a~purreqndescription ,a~banfn, a~bnfpo, a~badat,a~matnr, a~matkl, a~txz01, a~bsmng, a~meins, a~preis, a~lfdat, b~ps_psp_pnr, b~kostl,
          c~ebeln, c~ebelp, a~frgkz, a~frgzu, c~aedat, e~eindt, e~wemng, d~lifnr, g~name1, c~netpr, d~waers, d~wkurs, c~mwskz, c~netwr, f~belnr,
          f~buzei, f~budat, f~bwart, f~menge, f~dmbtr, f~wrbtr, f~gjahr
      INTO TABLE @it_ret
      FROM
          eban AS a
            LEFT JOIN
            ebkn AS b
            ON a~banfn = b~banfn
            AND a~bnfpo = b~bnfpo
            LEFT JOIN
            ekpo AS c
            ON a~banfn = c~banfn
            AND a~bnfpo = c~bnfpo
            AND c~loekz = ''
            LEFT JOIN
            ekko AS d
            ON c~ebeln = d~ebeln
            LEFT JOIN
            eket AS e
            ON c~ebeln = e~ebeln
            AND c~ebelp = e~ebelp
            LEFT JOIN
            ekbe AS f
            ON c~ebeln = f~ebeln
            AND c~ebelp = f~ebelp
            AND f~bewtp = 'E'
            LEFT JOIN
            lfa1 AS g
            ON d~lifnr = g~lifnr
          WHERE
            a~banfn IN @s_prnum
            AND a~badat IN @s_badat
            AND a~matnr IN @s_matnr
            AND a~ekgrp EQ @wa_agr51-low
            AND b~ps_psp_pnr IN @s_wbs
*      AND a~ernam EQ @sy-uname
      ORDER BY
          a~banfn,
          a~bnfpo.
          IF sy-subrc = 0.

            LOOP AT it_ret INTO wa_result.
              APPEND wa_result TO it_ret2.
            ENDLOOP.

          ENDIF.
        ENDIF.

      ELSEIF wa_agr51-low EQ '$EKGRP' .

        SELECT SINGLE low INTO @wa_agr52 FROM agr_1252 WHERE agr_name = @lv_con
          AND varbl = @wa_agr51-low.

        IF listbox = '2' OR listbox = 'X' OR listbox = ''.
          SELECT
            a~ernam, a~loekz, a~ekgrp, a~purreqndescription ,a~banfn, a~bnfpo, a~badat,a~matnr, a~matkl, a~txz01, a~bsmng, a~meins, a~preis, a~lfdat, b~ps_psp_pnr, b~kostl,
            c~ebeln, c~ebelp, a~frgkz, a~frgzu, c~aedat, e~eindt, e~wemng, d~lifnr, g~name1, c~netpr, d~waers, d~wkurs, c~mwskz, c~netwr, f~belnr,
            f~buzei, f~budat, f~bwart, f~menge, f~dmbtr, f~wrbtr, f~gjahr
        INTO TABLE @it_ret
        FROM
            eban AS a
            LEFT JOIN
            ebkn AS b
            ON a~banfn = b~banfn
            AND a~bnfpo = b~bnfpo
            LEFT JOIN
            ekpo AS c
            ON a~banfn = c~banfn
            AND a~bnfpo = c~bnfpo
            AND c~loekz = ''
            LEFT JOIN
            ekko AS d
            ON c~ebeln = d~ebeln
            LEFT JOIN
            eket AS e
            ON c~ebeln = e~ebeln
            AND c~ebelp = e~ebelp
            LEFT JOIN
            ekbe AS f
            ON c~ebeln = f~ebeln
            AND c~ebelp = f~ebelp
            AND f~bewtp = 'E'
            LEFT JOIN
            lfa1 AS g
            ON d~lifnr = g~lifnr
            WHERE
              a~banfn IN @s_prnum
              AND a~badat IN @s_badat
              AND a~matnr IN @s_matnr
              AND b~ps_psp_pnr IN @s_wbs
              AND a~frgkz EQ @listbox
              AND a~ekgrp EQ @wa_agr52
        ORDER BY
            a~banfn,
            a~bnfpo.
          IF sy-subrc = 0.

            LOOP AT it_ret INTO wa_result.
              APPEND wa_result TO it_ret2.
            ENDLOOP.

          ENDIF.
        ELSEIF listbox = '1'.

          SELECT
          a~ernam, a~loekz, a~ekgrp, a~purreqndescription ,a~banfn, a~bnfpo, a~badat,a~matnr, a~matkl, a~txz01, a~bsmng, a~meins, a~preis, a~lfdat, b~ps_psp_pnr, b~kostl,
          c~ebeln, c~ebelp, a~frgkz, a~frgzu, c~aedat, e~eindt, e~wemng, d~lifnr, g~name1, c~netpr, d~waers, d~wkurs, c~mwskz, c~netwr, f~belnr,
          f~buzei, f~budat, f~bwart, f~menge, f~dmbtr, f~wrbtr, f~gjahr
      INTO TABLE @it_ret
      FROM
          eban AS a
            LEFT JOIN
            ebkn AS b
            ON a~banfn = b~banfn
            AND a~bnfpo = b~bnfpo
            LEFT JOIN
            ekpo AS c
            ON a~banfn = c~banfn
            AND a~bnfpo = c~bnfpo
            AND c~loekz = ''
            LEFT JOIN
            ekko AS d
            ON c~ebeln = d~ebeln
            LEFT JOIN
            eket AS e
            ON c~ebeln = e~ebeln
            AND c~ebelp = e~ebelp
            LEFT JOIN
            ekbe AS f
            ON c~ebeln = f~ebeln
            AND c~ebelp = f~ebelp
            AND f~bewtp = 'E'
            LEFT JOIN
            lfa1 AS g
            ON d~lifnr = g~lifnr
          WHERE
            a~banfn IN @s_prnum
            AND a~badat IN @s_badat
            AND a~matnr IN @s_matnr
            AND a~ekgrp EQ @wa_agr52
            AND b~ps_psp_pnr IN @s_wbs
*      AND a~ernam EQ @sy-uname
      ORDER BY
          a~banfn,
          a~bnfpo.
        ENDIF.
        IF sy-subrc = 0.

          LOOP AT it_ret INTO wa_result.
            APPEND wa_result TO it_ret2.
          ENDLOOP.

        ENDIF.

      ENDIF.
    ENDLOOP.

*  ELSEIF WA_AGR-LOW = $EKGRP .
**
**    LOOP AT it_agr51 INTO wa_agr51.
**      IF wa_agr51-low = '$EKGRP'.
*        SELECT SINGLE low INTO @wa_agr52 FROM agr_1252 WHERE agr_name = @lv_con
*          AND varbl = @wa_agr51.
**      ENDIF.
*
*      LOOP AT .
*
*      ENDLOOP.
*    ENDLOOP.

  ENDIF.

*  IF wa_agr51 = '$EKGRP'.
*    SELECT low INTO @DATA(wa_agr52) FROM agr_1252 WHERE agr_name = @lv_con
*      AND varbl = @wa_agr51.
*  ENDIF.



  LOOP AT it_ret2 INTO DATA(wa_ret).
    MOVE-CORRESPONDING wa_ret TO wa_data.

    DATA : lv_name TYPE usr03-name1.
    DATA : lv_isvc TYPE esll-srvpos.
    DATA : lv_svctext TYPE esll-ktext1.
    DATA : lv_svcuom TYPE esll-meins.
    DATA : lv_svcqty TYPE esll-menge.
    DATA : lv_svcprc TYPE esll-brtwr.

* Username
    CALL FUNCTION 'UMB_SERVICE_OWNER_NAME'
      EXPORTING
        i_owner = wa_data-ernam
*       IX_CHECK_EXISTENCE       = ' '
      IMPORTING
        e_name  = wa_data-uname
*   EXCEPTIONS
*       NOT_FOUND                = 1
*       OTHERS  = 2
      .

    IF sy-subrc <> 0.
* Implement suitable error handling here
    ENDIF.

* Item Service
    SELECT SINGLE pstyp INTO @DATA(wa_icat) "--> get value for Item Category
    FROM eban
    WHERE
      banfn = @wa_data-banfn
      AND bnfpo = @wa_data-bnfpo.

    IF wa_icat EQ 9. "Item category --> Service
      SELECT SINGLE el~srvpos, el~ktext1, el~meins, el~menge, el~tbtwr
      INTO
       (
        @lv_isvc,
        @lv_svctext,
        @lv_svcuom,
        @lv_svcqty,
        @lv_svcprc
       )
      FROM
        esll AS el
        INNER JOIN
        eslh AS eh
        ON el~packno = eh~packno
        WHERE
          eh~ebeln EQ @wa_data-banfn
          AND eh~ebelp EQ @wa_data-bnfpo
          AND el~package NE 'X'.

      IF wa_data-bsmng NE 0.
        wa_data-openqty = 0.
      ELSE.
        wa_data-openqty = lv_svcqty.
      ENDIF.

      wa_data-matnr = lv_isvc.
      wa_data-meins = lv_svcuom.
      wa_data-txz01 = lv_svctext.
      wa_data-menge = lv_svcqty.
      wa_data-preis = lv_svcprc.

    ELSE.
      SELECT SINGLE menge INTO @wa_data-menge FROM eban
         WHERE banfn EQ @wa_data-banfn
          AND bnfpo EQ @wa_data-bnfpo.

      wa_data-openqty = wa_data-menge - wa_data-bsmng.
    ENDIF.

* Quantity EKPO
    SELECT SINGLE menge INTO @wa_data-qtypo FROM ekpo
      WHERE ebeln EQ @wa_data-ebeln
      AND ebelp EQ @wa_data-ebelp.

* Release Date PR
    SELECT SINGLE objectclas, objectid, changenr, tabname, tabkey, fname, value_new INTO @DATA(wa_reldate)
      FROM cdpos
      WHERE objectid EQ @wa_data-banfn
      AND objectclas EQ 'BANF'
      AND tabname EQ 'EBAN'
      AND fname EQ 'FRGKZ'
      AND value_new EQ '2'.

    SELECT SINGLE udate INTO @DATA(wa_release)
    FROM cdhdr
    WHERE objectclas EQ @wa_reldate-objectclas
    AND objectid EQ @wa_reldate-objectid
    AND changenr EQ @wa_reldate-changenr.

*Status Release PR
    IF wa_ret-frgkz EQ '2'.
      wa_data-appr3 = 'RELEASED'.
      wa_data-frgdt = wa_release.
    ELSE.
      wa_data-appr3 = 'INCOMPLETE'.
      wa_data-frgdt = ''.
    ENDIF.


    CALL FUNCTION 'BAPI_REQUISITION_GETRELINFO'
      EXPORTING
        number                 = wa_data-banfn
*       ITEM                   =
*       REL_CODE               =
      TABLES
*       GENERAL_RELEASE_INFO   =
*       RELEASE_PREREQUISITES  =
        release_already_posted = ls_posted
        release_final          = lt_final
*       RETURN                 =
      .

*Progress Approval
    IF wa_ret-frgzu EQ 'X'.
      wa_data-appr1 = ls_posted-rel_cd_tx1.
    ELSEIF wa_ret-frgzu EQ 'XX'.
      wa_data-appr1 = ls_posted-rel_cd_tx2.
    ELSEIF wa_ret-frgzu EQ 'XXX'.
      wa_data-appr1 = ls_posted-rel_cd_tx3.
    ELSEIF wa_ret-frgzu EQ 'XXXX'.
      wa_data-appr1 = ls_posted-rel_cd_tx4.
    ELSEIF wa_ret-frgzu EQ 'XXXXX'.
      wa_data-appr1 = ls_posted-rel_cd_tx5.
    ELSEIF wa_ret-frgzu EQ 'XXXXXX'.
      wa_data-appr1 = ls_posted-rel_cd_tx6.
    ENDIF.

* PO Rate
    IF wa_ret-waers NE 'IDR'.
      wa_data-wkurs = wa_ret-wkurs * 100.
    ENDIF.

*    PO tax code
    IF wa_ret-mwskz NE ''.
      SELECT SINGLE text1 INTO @DATA(wa_taxdescr) FROM t007s
      WHERE
        spras = 'E'
        AND kalsm = 'TAXID'
        AND mwskz = @wa_ret-mwskz.

      wa_data-taxdescr = wa_taxdescr.
    ENDIF.

* Total Amount
    TRY.
        wa_data-totalamnt = wa_data-preis * wa_data-menge.
      CATCH cx_sy_arithmetic_overflow.
    ENDTRY.

* Approval Date Kasie
    SELECT SINGLE objectclas, objectid, changenr, tabname, tabkey, fname, value_new INTO @DATA(wa_kasie)
      FROM cdpos
      WHERE objectid EQ @wa_data-banfn
      AND objectclas EQ 'BANF'
      AND tabname EQ 'EBAN'
      AND fname EQ 'FRGZU'
      AND value_new EQ 'X'.

    SELECT SINGLE udate INTO @wa_data-kasie
      FROM cdhdr
      WHERE objectclas EQ @wa_kasie-objectclas
      AND objectid EQ @wa_kasie-objectid
      AND changenr EQ @wa_kasie-changenr.

    IF ls_posted-rel_code1 IS INITIAL.
      wa_data-kasie = ''.
    ENDIF.

* Approval Date Kadept
    SELECT SINGLE objectclas, objectid, changenr, tabname, tabkey, fname, value_new INTO @DATA(wa_kadept)
      FROM cdpos
      WHERE objectid EQ @wa_data-banfn
      AND objectclas EQ 'BANF'
      AND tabname EQ 'EBAN'
      AND fname EQ 'FRGZU'
      AND value_new EQ 'XX'.

    SELECT SINGLE udate INTO @wa_data-kadept
      FROM cdhdr
      WHERE objectclas EQ @wa_kadept-objectclas
      AND objectid EQ @wa_kadept-objectid
      AND changenr EQ @wa_kadept-changenr.

    IF ls_posted-rel_code2 IS INITIAL.
      wa_data-kadept = ''.
    ENDIF.

* Approval Date Kadiv
    SELECT SINGLE objectclas, objectid, changenr, tabname, tabkey, fname, value_new INTO @DATA(wa_kadiv)
      FROM cdpos
      WHERE objectid EQ @wa_data-banfn
      AND objectclas EQ 'BANF'
      AND tabname EQ 'EBAN'
      AND fname EQ 'FRGZU'
      AND value_new EQ 'XXX'.

    SELECT SINGLE udate INTO @wa_data-kadiv
      FROM cdhdr
      WHERE objectclas EQ @wa_kadiv-objectclas
      AND objectid EQ @wa_kadiv-objectid
      AND changenr EQ @wa_kadiv-changenr.

    IF ls_posted-rel_code3 IS INITIAL.
      wa_data-kadiv = ''.
    ENDIF.

* Approval Date DIR 1
    SELECT SINGLE objectclas, objectid, changenr, tabname, tabkey, fname, value_new INTO @DATA(wa_dir1)
      FROM cdpos
      WHERE objectid EQ @wa_data-banfn
      AND objectclas EQ 'BANF'
      AND tabname EQ 'EBAN'
      AND fname EQ 'FRGZU'
      AND value_new EQ 'XXXX'.

    SELECT SINGLE udate INTO @wa_data-dir1
      FROM cdhdr
      WHERE objectclas EQ @wa_dir1-objectclas
      AND objectid EQ @wa_dir1-objectid
      AND changenr EQ @wa_dir1-changenr.

    IF ls_posted-rel_code4 IS INITIAL.
      wa_data-dir1 = ''.
    ENDIF.

* Approval Date DIR 2
    SELECT SINGLE objectclas, objectid, changenr, tabname, tabkey, fname, value_new INTO @DATA(wa_dir2)
      FROM cdpos
      WHERE objectid EQ @wa_data-banfn
      AND objectclas EQ 'BANF'
      AND tabname EQ 'EBAN'
      AND fname EQ 'FRGZU'
      AND value_new EQ 'XXXXX'.

    SELECT SINGLE udate INTO @wa_data-dir2
      FROM cdhdr
      WHERE objectclas EQ @wa_dir2-objectclas
      AND objectid EQ @wa_dir2-objectid
      AND changenr EQ @wa_dir2-changenr.

    IF ls_posted-rel_code5 IS INITIAL.
      wa_data-dir2 = ''.
    ENDIF.

* Approval Date DIR 3
    SELECT SINGLE objectclas, objectid, changenr, tabname, tabkey, fname, value_new INTO @DATA(wa_dir3)
      FROM cdpos
      WHERE objectid EQ @wa_data-banfn
      AND objectclas EQ 'BANF'
      AND tabname EQ 'EBAN'
      AND fname EQ 'FRGZU'
      AND value_new EQ 'XXXXXX'.

    SELECT SINGLE udate INTO @wa_data-dir3
      FROM cdhdr
      WHERE objectclas EQ @wa_dir3-objectclas
      AND objectid EQ @wa_dir3-objectid
      AND changenr EQ @wa_dir3-changenr.

    IF ls_posted-rel_code6 IS INITIAL.
      wa_data-dir3 = ''.
    ENDIF.

* Target Approval
    DESCRIBE TABLE ls_posted LINES itline.
    READ TABLE ls_posted INTO DATA(wa_tarappr) INDEX itline.

*    IF ls_posted-rel_code6 IS NOT INITIAL.
*      wa_data-appr2 = wa_tarappr-rel_cd_tx6.
*    ELSEIF ls_posted-rel_code5 IS NOT INITIAL.
*      wa_data-appr2 = wa_tarappr-rel_cd_tx5.
*    ELSEIF ls_posted-rel_code4 IS NOT INITIAL.
*      wa_data-appr2 = wa_tarappr-rel_cd_tx4.
*    ELSEIF ls_posted-rel_code3 IS NOT INITIAL.
*      wa_data-appr2 = wa_tarappr-rel_cd_tx3.
*    ELSEIF ls_posted-rel_code2 IS NOT INITIAL.
*      wa_data-appr2 = wa_tarappr-rel_cd_tx2.
*    ELSEIF ls_posted-rel_code1 IS NOT INITIAL.
*      wa_data-appr2 = wa_tarappr-rel_cd_tx1.
*    ENDIF.

    CALL FUNCTION 'BAPI_REQUISITION_GETRELINFO'
      EXPORTING
        number                 = wa_data-banfn
*       ITEM                   =
*       REL_CODE               =
      TABLES
*       GENERAL_RELEASE_INFO   =
*       RELEASE_PREREQUISITES  =
        release_already_posted = ls_posted
        release_final          = lt_final
*       RETURN                 =
      .

    READ TABLE lt_final INTO ls_final INDEX 1.
    IF ls_final-rel_code6 IS NOT INITIAL.
      wa_data-appr2 = ls_final-rel_cd_tx6.
    ELSEIF ls_final-rel_code5 IS NOT INITIAL.
      wa_data-appr2 = ls_final-rel_cd_tx5.
    ELSEIF ls_final-rel_code4 IS NOT INITIAL.
      wa_data-appr2 = ls_final-rel_cd_tx4.
    ELSEIF ls_final-rel_code3 IS NOT INITIAL.
      wa_data-appr2 = ls_final-rel_cd_tx3.
    ELSEIF ls_final-rel_code2 IS NOT INITIAL.
      wa_data-appr2 = ls_final-rel_cd_tx2.
    ELSEIF ls_final-rel_code1 IS NOT INITIAL.
      wa_data-appr2 = ls_final-rel_cd_tx1.
    ENDIF.

*    Quantity Ekbe
    SELECT SINGLE menge INTO @wa_data-qtygr
            FROM ekbe
            WHERE
            ebeln = @wa_ret-ebeln
            AND ebelp = @wa_ret-ebelp
      AND belnr = @wa_data-belnr
      AND buzei = @wa_data-buzei
      AND gjahr = @wa_ret-gjahr.

    IF wa_ret-bwart EQ '102'. "--> movement type reversal (GR correct)
      wa_data-qtygr = wa_data-qtygr * -1.
      wa_data-dmbtr = wa_data-dmbtr * -1.
      wa_data-wrbtr = wa_data-wrbtr * -1.
    ENDIF.


*   PO Release Status
    SELECT SINGLE frgke INTO @DATA(wa_postat) FROM ekko WHERE ebeln = @wa_data-ebeln.
    IF wa_data-ebeln IS NOT INITIAL.
      IF wa_postat EQ 'R'.
        wa_data-postat = 'RELEASED'.
      ELSE.
        wa_data-postat = 'INCOMPLETE'.
      ENDIF.
    ENDIF.

*   PO Release Date
    IF wa_postat EQ 'R'.
      SELECT SINGLE objectclas, objectid, changenr, tabname, tabkey, fname, value_new INTO @DATA(wa_porel)
        FROM cdpos
        WHERE objectid EQ @wa_data-ebeln
        AND objectclas EQ 'EINKBELEG'
        AND tabname EQ 'EKKO'
        AND fname EQ 'FRGKE'
        AND value_new EQ 'R'.

      SELECT SINGLE udate INTO @wa_data-poreldate
        FROM cdhdr
        WHERE objectclas EQ @wa_porel-objectclas
        AND objectid EQ @wa_porel-objectid
        AND changenr EQ @wa_porel-changenr.
    ENDIF.

*  Quantity PO
    wa_data-netp = wa_data-qtypo - wa_data-wemng.

    APPEND wa_data TO it_data.
    CLEAR : wa_tarappr, ls_posted, itline, wa_data, wa_postat, wa_porel.
*    MODIFY it_data FROM wa_data TRANSPORTING uname.

  ENDLOOP.

ENDFORM.

*&---------------------------------------------------------------------*
*& Form f_display_data
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*

FORM f_display_data.

  DATA: lx_msg TYPE REF TO cx_salv_msg.
  TRY.
      cl_salv_table=>factory(
      IMPORTING
        r_salv_table = o_alv
        CHANGING
          t_table      = it_data ).
    CATCH cx_salv_msg INTO lx_msg.
  ENDTRY.

  PERFORM f_column_setting.

  o_alv->get_functions( )->set_all( abap_true ).
  o_alv->display( ).

ENDFORM.

*&---------------------------------------------------------------------*
*& Form f_column_setting
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*

FORM f_column_setting.

  DATA: lo_column  TYPE REF TO cl_salv_column_table.

  o_alv->get_columns( )->set_optimize( value = if_salv_c_bool_sap=>true ).

  TRY .
      o_alv->get_columns( )->get_column( columnname = 'NETPR' :

*      )->set_short_text( value = 'Prod Order' ),
*      )->set_medium_text( value = 'Prod Order' ),
*      )->set_long_text( value = 'Prod Order' ),
      )->set_currency_column( value = 'WAERS' ).

    CATCH cx_salv_not_found.
    CATCH cx_salv_data_error.
  ENDTRY.

  TRY .
      o_alv->get_columns( )->get_column( columnname = 'NETWR' :

*      )->set_short_text( value = 'Prod Order' ),
*      )->set_medium_text( value = 'Prod Order' ),
*      )->set_long_text( value = 'Prod Order' ),
      )->set_currency_column( value = 'WAERS' ).

    CATCH cx_salv_not_found.
    CATCH cx_salv_data_error.
  ENDTRY.

  TRY .
      o_alv->get_columns( )->get_column( columnname = 'PREIS' :

*      )->set_short_text( value = 'Prod Order' ),
*      )->set_medium_text( value = 'Prod Order' ),
*      )->set_long_text( value = 'Prod Order' ),
      )->set_currency( value = 'IDR' ).

    CATCH cx_salv_not_found.
    CATCH cx_salv_data_error.
  ENDTRY.

  TRY .
      o_alv->get_columns( )->get_column( columnname = 'TOTALAMNT' :

      )->set_short_text( value = 'Total' ),
      )->set_medium_text( value = 'Total' ),
      )->set_long_text( value = 'Total Amount' ),
      )->set_currency( value = 'IDR' ).

    CATCH cx_salv_not_found.
    CATCH cx_salv_data_error.
  ENDTRY.

  TRY .
      o_alv->get_columns( )->get_column( columnname = 'MENGE' :

*      )->set_short_text( value = 'Prod Order' ),
*      )->set_medium_text( value = 'Prod Order' ),
*      )->set_long_text( value = 'Prod Order' ),
      )->set_quantity_column( value = 'MEINS' ).

    CATCH cx_salv_not_found.
    CATCH cx_salv_data_error.
  ENDTRY.

  TRY .
      o_alv->get_columns( )->get_column( columnname = 'OPENQTY' :

      )->set_short_text( value = 'OpQty' ),
      )->set_medium_text( value = 'OpQty' ),
      )->set_long_text( value = 'Open Qty' ),
      )->set_quantity_column( value = 'MEINS' ).

    CATCH cx_salv_not_found.
    CATCH cx_salv_data_error.
  ENDTRY.

  TRY .
      o_alv->get_columns( )->get_column( columnname = 'QTYPO' :

*      )->set_short_text( value = 'Prod Order' ),
*      )->set_medium_text( value = 'Prod Order' ),
*      )->set_long_text( value = 'Prod Order' ),
      )->set_quantity_column( value = 'MEINS' ).

    CATCH cx_salv_not_found.
    CATCH cx_salv_data_error.
  ENDTRY.

*  TRY .
*      o_alv->get_columns( )->get_column( columnname = 'QTYGR' :
*
**      )->set_short_text( value = 'Prod Order' ),
**      )->set_medium_text( value = 'Prod Order' ),
**      )->set_long_text( value = 'Prod Order' ),
*      )->set_quantity_column( value = 'MEINS' ).
*
*    CATCH cx_salv_not_found.
*    CATCH cx_salv_data_error.
*  ENDTRY.

  TRY .
      o_alv->get_columns( )->get_column( columnname = 'LIFNR' :

*      )->set_short_text( value = 'Prod Order' ),
*      )->set_medium_text( value = 'Prod Order' ),
*      )->set_long_text( value = 'Prod Order' ),
      )->set_visible( value = '' ).

    CATCH cx_salv_not_found.
    CATCH cx_salv_data_error.
  ENDTRY.

  TRY .
      o_alv->get_columns( )->get_column( columnname = 'NAME1' :

      )->set_short_text( value = 'Supp' ),
      )->set_medium_text( value = 'Supp' ),
      )->set_long_text( value = 'Supplier Name' ).
*      )->set_visible( value = '' ).

    CATCH cx_salv_not_found.
    CATCH cx_salv_data_error.
  ENDTRY.

  TRY .
      o_alv->get_columns( )->get_column( columnname = 'BSMNG' :

*      )->set_short_text( value = 'Supp' ),
*      )->set_medium_text( value = 'Supp' ),
*      )->set_long_text( value = 'Supplier Name' ).
      )->set_visible( value = '' ).

    CATCH cx_salv_not_found.
    CATCH cx_salv_data_error.
  ENDTRY.

  TRY .
      o_alv->get_columns( )->get_column( columnname = 'WKURS' :

*      )->set_short_text( value = 'Prod Order' ),
*      )->set_medium_text( value = 'Prod Order' ),
*      )->set_long_text( value = 'Prod Order' ),
      )->set_currency_column( value = 'WAERS' ).

    CATCH cx_salv_not_found.
    CATCH cx_salv_data_error.
  ENDTRY.

  TRY .
      o_alv->get_columns( )->get_column( columnname = 'APPR1' :

      )->set_short_text( value = 'Prog.Appr' ),
      )->set_medium_text( value = 'Prog.Appr' ),
      )->set_long_text( value = 'Progress Approval' ).

    CATCH cx_salv_not_found.
    CATCH cx_salv_data_error.
  ENDTRY.

  TRY .
      o_alv->get_columns( )->get_column( columnname = 'APPR2' :

      )->set_short_text( value = 'Targ.Appr' ),
      )->set_medium_text( value = 'Targ.Appr' ),
      )->set_long_text( value = 'Target Approval' ).

    CATCH cx_salv_not_found.
    CATCH cx_salv_data_error.
  ENDTRY.

  TRY .
      o_alv->get_columns( )->get_column( columnname = 'APPR3' :

      )->set_short_text( value = 'Stat.Rel' ),
      )->set_medium_text( value = 'Stat.Rel' ),
      )->set_long_text( value = 'Status Release PR' ).

    CATCH cx_salv_not_found.
    CATCH cx_salv_data_error.
  ENDTRY.

  TRY .
      o_alv->get_columns( )->get_column( columnname = 'KASIE' :

      )->set_short_text( value = 'Kasie PR' ),
      )->set_medium_text( value = 'Kasie PR' ),
      )->set_long_text( value = 'Kasie PR' ).

    CATCH cx_salv_not_found.
    CATCH cx_salv_data_error.
  ENDTRY.

  TRY .
      o_alv->get_columns( )->get_column( columnname = 'KADEPT' :

      )->set_short_text( value = 'KaDept PR' ),
      )->set_medium_text( value = 'KaDept PR' ),
      )->set_long_text( value = 'KaDept PR' ).

    CATCH cx_salv_not_found.
    CATCH cx_salv_data_error.
  ENDTRY.

  TRY .
      o_alv->get_columns( )->get_column( columnname = 'KADIV' :

      )->set_short_text( value = 'KaDiv PR' ),
      )->set_medium_text( value = 'KaDiv PR' ),
      )->set_long_text( value = 'KaDiv PR' ).

    CATCH cx_salv_not_found.
    CATCH cx_salv_data_error.
  ENDTRY.

  TRY .
      o_alv->get_columns( )->get_column( columnname = 'DIR1' :

      )->set_short_text( value = 'DIR PR 1' ),
      )->set_medium_text( value = 'DIR PR 1' ),
      )->set_long_text( value = 'DIR PR 1' ).

    CATCH cx_salv_not_found.
    CATCH cx_salv_data_error.
  ENDTRY.

  TRY .
      o_alv->get_columns( )->get_column( columnname = 'DIR2' :

      )->set_short_text( value = 'DIR PR 2' ),
      )->set_medium_text( value = 'DIR PR 2' ),
      )->set_long_text( value = 'DIR PR 2' ).

    CATCH cx_salv_not_found.
    CATCH cx_salv_data_error.
  ENDTRY.

  TRY .
      o_alv->get_columns( )->get_column( columnname = 'DIR3' :

      )->set_short_text( value = 'DIR PR 3' ),
      )->set_medium_text( value = 'DIR PR 3' ),
      )->set_long_text( value = 'DIR PR 3' ).

    CATCH cx_salv_not_found.
    CATCH cx_salv_data_error.
  ENDTRY.

  TRY .
      o_alv->get_columns( )->get_column( columnname = 'POSTAT' :

      )->set_short_text( value = 'PO St' ),
      )->set_medium_text( value = 'PO Stat' ),
      )->set_long_text( value = 'PO Status' ).

    CATCH cx_salv_not_found.
    CATCH cx_salv_data_error.
  ENDTRY.

  TRY .
      o_alv->get_columns( )->get_column( columnname = 'PORELDATE' :

      )->set_short_text( value = 'PO R' ),
      )->set_medium_text( value = 'PO Rel' ),
      )->set_long_text( value = 'PO Rel Date' ).

    CATCH cx_salv_not_found.
    CATCH cx_salv_data_error.
  ENDTRY.

  TRY .
      o_alv->get_columns( )->get_column( columnname = 'QTYGR' :

      )->set_short_text( value = 'Qty' ),
      )->set_medium_text( value = 'Qty GR' ),
      )->set_long_text( value = 'Qty GR' ),
      )->set_quantity_column( value = 'MEINS' ).

    CATCH cx_salv_not_found.
    CATCH cx_salv_data_error.
  ENDTRY.

  TRY .
      o_alv->get_columns( )->get_column( columnname = 'NETP' :

      )->set_short_text( value = 'OpQty' ),
      )->set_medium_text( value = 'OpenQty' ),
      )->set_long_text( value = 'Open Qty PO' ).

    CATCH cx_salv_not_found.
    CATCH cx_salv_data_error.
  ENDTRY.

  TRY .
      o_alv->get_columns( )->get_column( columnname = 'WEMNG' :

*      )->set_short_text( value = 'Prod Order' ),
*      )->set_medium_text( value = 'Prod Order' ),
*      )->set_long_text( value = 'Prod Order' ),
      )->set_visible( value = '' ).

    CATCH cx_salv_not_found.
    CATCH cx_salv_data_error.
  ENDTRY.

ENDFORM.
