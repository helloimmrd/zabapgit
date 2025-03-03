*&---------------------------------------------------------------------*
*& Include          YSRILOP_R00033_F01
*&---------------------------------------------------------------------*

FORM f_get_data.

  DATA: lv_lifnr TYPE c LENGTH 10.

  SELECT a~ebeln, a~lifnr, b~name1, a~revno, a~aedat, a~ekgrp, a~waers, a~frgke
      INTO TABLE @DATA(it_result)
      FROM ekko AS a
      INNER JOIN lfa1 AS b ON a~lifnr EQ b~lifnr
      WHERE a~ebeln IN @p_ebeln
      AND a~ekgrp IN @p_ekgrp
      AND a~aedat IN @p_aedat.
*      update by SUR
*      AND ( a~frgke EQ 'G' OR a~frgke EQ 'R'
*      OR a~frgke EQ ' '  ).

  SORT it_result BY ebeln ASCENDING.

  LOOP AT it_result INTO DATA(wa_stat).
    MOVE-CORRESPONDING wa_stat TO wa_data.

    SELECT SINGLE ebeln INTO @DATA(wa_ebeln) FROM zsrift_0001
      WHERE ebeln = @wa_data-ebeln.

    IF sy-subrc = 0.
      wa_data-status = 'Document Already Sent ! '.
*      wa_data-icon = icon_cancel.
    ELSE.
      wa_data-status = '-'.
    ENDIF.

    IF wa_data-frgke = 'R' OR wa_data-frgke = 'G'.
      wa_data-frgke = 'Released'.
    ELSE.
      wa_data-frgke = 'In Process'.
    ENDIF.

    SHIFT wa_data-lifnr LEFT DELETING LEADING '0'.

    lv_lifnr = |0000{ wa_data-lifnr }|.

*    field email
    SELECT SINGLE addrcomm INTO @DATA(wa_but) FROM but000
    WHERE partner = @lv_lifnr.

    " List of 'TO' Email
*    SELECT SINGLE smtp_addr INTO @wa_data-email FROM adr6
*      WHERE addrnumber = @wa_but
*      AND flg_nouse = ''.

    SELECT SINGLE email_addr INTO @wa_data-email FROM zsrifdt_00024
          WHERE email_dest_type EQ 1
          AND lifnr EQ @lv_lifnr
          and is_active EQ 'Y'.

    IF wa_data-email IS INITIAL.
      wa_data-status = 'NO Email'.
      wa_data-icon = icon_cancel.
    ELSEIF wa_data-status = 'Document Already Sent ! '.
      wa_data-icon = icon_cancel.
    ELSEIF wa_data-email IS NOT INITIAL.
      wa_data-icon = icon_okay.
    ENDIF.

*    MODIFY it_data FROM wa_stat TRANSPORTING status icon lifnr.
    APPEND wa_data TO it_data.
*    CLEAR : lv_lifnr.

  ENDLOOP.

ENDFORM.

FORM f_data_smartform.

  DATA: lv_count     TYPE i,
        lv_count_str TYPE string.

  LOOP AT it_data INTO wa_pdf.

    SELECT SINGLE ebeln, revno, aedat, zterm, waers, knumv
        INTO (@t_header-ebeln, @t_header-revno, @t_header-aedat, @t_header-zterm, @t_header-waers, @t_header-knumv)
        FROM ekko AS a
        WHERE ebeln EQ @wa_pdf-ebeln
        AND ( frgke EQ 'G' OR frgke EQ 'R'
        OR frgke EQ ' '  ).

    SELECT ebeln, revno, aedat, zterm, waers, knumv
    INTO TABLE @DATA(dt_header)
    FROM ekko
    WHERE ebeln EQ @wa_pdf-ebeln
    AND ( frgke EQ 'G' OR frgke EQ 'R' OR frgke EQ ' ' ).

    SORT dt_header[] BY ebeln ASCENDING.

    LOOP AT dt_header INTO DATA(wa_hdr).

      IF t_header IS NOT INITIAL.
        SELECT SINGLE b~name1, b~name2, b~stras, b~telfx, b~adrnr, a~lifnr, d~str_suppl1, d~str_suppl2, d~mc_city1, d~region, a~rlwrt
          INTO (@t_header-name1, @t_header-name2, @t_header-alamat, @t_header-telfx, @t_header-adrnr, @t_header-lifnr, @t_header-str_suppl1, @t_header-str_suppl2, @t_header-mc_city1, @t_header-region, @t_header-subtotal)
          FROM ekko AS a
          INNER JOIN lfa1 AS b ON a~lifnr EQ b~lifnr
          INNER JOIN adrc AS d ON b~adrnr EQ d~addrnumber
          WHERE a~ebeln EQ @wa_hdr-ebeln.

        SELECT SINGLE bezei
          INTO @t_header-bezei
          FROM t005u
          WHERE spras EQ @sy-langu AND land1 EQ 'ID' AND bland EQ @t_header-region.

        SELECT SINGLE verkf
          INTO @t_header-verkf
          FROM lfm1
          WHERE lifnr EQ @t_header-lifnr.

        SELECT SINGLE smtp_addr
          INTO @t_header-smtp_addr
          FROM adr6
          WHERE addrnumber EQ @t_header-adrnr.

        SELECT SINGLE ztag1
          INTO @DATA(lv_term)
          FROM t052
          WHERE zterm EQ @t_header-zterm.

        SHIFT lv_term LEFT DELETING LEADING '0'.

        lv_top = |{ lv_term } Days|.

        DATA: v_thead  TYPE tdobname,
              notes    TYPE TABLE OF tline,
              ls_notes TYPE tline.

        CLEAR v_thead.
        CLEAR notes.

        v_thead = wa_hdr-ebeln.

        DATA : lv_line  TYPE tline,
               lv_index TYPE sy-tabix.

        CALL FUNCTION 'READ_TEXT'
          EXPORTING
            id                      = 'F01'
            language                = sy-langu
            name                    = v_thead
            object                  = 'EKKO'
          TABLES
            lines                   = notes
          EXCEPTIONS
            id                      = 1
            language                = 2
            name                    = 3
            not_found               = 4
            object                  = 5
            reference_check         = 6
            wrong_access_to_archive = 7
            OTHERS                  = 8.
        IF sy-subrc <> 0.
        ENDIF.

        READ TABLE notes INDEX 2 INTO t_header-podesc.
        LOOP AT notes INTO lv_line.
          CONCATENATE ls_notes lv_line INTO ls_notes SEPARATED BY space.
        ENDLOOP.
        t_header-remarks = ls_notes-tdline.

        SELECT mwskz INTO TABLE @lt_mwskz2 FROM ekpo
          WHERE ebeln EQ @t_header-ebeln.

        DELETE lt_mwskz2 WHERE mwskz IS INITIAL.
        DELETE ADJACENT DUPLICATES FROM lt_mwskz2.

        SELECT SINGLE knumh INTO @DATA(lv_knumh)
          FROM a003
          WHERE kappl EQ 'TX' AND kschl EQ 'MWVS' AND aland EQ 'ID' AND mwskz EQ @lt_mwskz.

        SELECT SINGLE kbetr INTO @DATA(lv_kbetr) FROM konp
          WHERE knumh EQ @lv_knumh.

        t_header-ppn = lv_kbetr / 10.

        DATA: ls_result TYPE zsriltt_00008,
              lt_result TYPE TABLE OF zsriltt_00008,
              lv_menge  TYPE p LENGTH 15,
              lv_amount TYPE p LENGTH 15 DECIMALS 0,
              lv_row    TYPE i,
              lv_idx    TYPE i.

        lv_idx = 0.

        "MASUK SELECT TABLE

        SELECT a~ebeln, a~ebelp, a~matnr, a~txz01, a~menge, a~netpr, a~netwr, a~meins, a~pstyp, a~knttp, a~packno, a~lgort, a~mwskz "add lgort dan mwskz by HN 23/4/2024
          INTO TABLE @DATA(t_ekpo)
          FROM ekpo AS a
          WHERE a~ebeln EQ @wa_hdr-ebeln
          AND a~loekz EQ ''.

        SORT t_ekpo[] BY ebelp ASCENDING.

        MOVE-CORRESPONDING t_ekpo TO t_result.
        DELETE ADJACENT DUPLICATES FROM t_result.

        DATA: v_thead2     TYPE tdobname,
              v_thead3     TYPE tdobname, "add by HN 12008 11/7/2024
              v_thead4     TYPE tdobname, "add by HN 12008 15/7/2024
              po_text      TYPE TABLE OF tline,
              matpo_text   TYPE TABLE OF tline,
              lv_potext    TYPE tline,
              item_text    TYPE TABLE OF tline,
              lv_itemtext  TYPE tline,
              deliv_text   TYPE TABLE OF tline,
              lv_delivtext TYPE tline.

        LOOP AT t_ekpo INTO DATA(data_table).

          IF data_table-knttp EQ 'U'.
            SELECT SINGLE packno
              INTO @DATA(lv_packno)
              FROM ekbe
              WHERE ebeln EQ @wa_hdr-ebeln AND vgabe EQ 1.

            data_table-packno = lv_packno.
          ELSE.
            data_table-packno = data_table-packno.
          ENDIF.

          SELECT SINGLE eindt
            INTO @DATA(lv_eindt)
            FROM eket
            WHERE ebeln EQ @wa_hdr-ebeln AND ebelp EQ @data_table-ebelp.

          "Item Description
          CLEAR v_thead2.
          CLEAR matpo_text.

          v_thead2 = |{ wa_hdr-ebeln }{ data_table-ebelp }|.
          ls_result-ebeln = wa_hdr-ebeln.

          CALL FUNCTION 'READ_TEXT'
            EXPORTING
              id                      = 'F03'
              language                = sy-langu
              name                    = v_thead2
              object                  = 'EKPO'
            TABLES
              lines                   = matpo_text
            EXCEPTIONS
              id                      = 1
              language                = 2
              name                    = 3
              not_found               = 4
              object                  = 5
              reference_check         = 6
              wrong_access_to_archive = 7
              OTHERS                  = 8.
          IF sy-subrc <> 0.
* Implement suitable error handling here
          ENDIF.

          IF matpo_text IS NOT INITIAL.

            CLEAR lv_potext.
            LOOP AT matpo_text INTO lv_line.
              CONCATENATE lv_potext lv_line INTO lv_potext SEPARATED BY space.
            ENDLOOP.

          ELSE.

            CLEAR v_thead4.
            CLEAR po_text.

            v_thead4 = data_table-matnr.

            CALL FUNCTION 'READ_TEXT'
              EXPORTING
                id                      = 'BEST'
                language                = sy-langu
                name                    = v_thead4
                object                  = 'MATERIAL'
              TABLES
                lines                   = po_text
              EXCEPTIONS
                id                      = 1
                language                = 2
                name                    = 3
                not_found               = 4
                object                  = 5
                reference_check         = 6
                wrong_access_to_archive = 7
                OTHERS                  = 8.
            IF sy-subrc <> 0.
            ENDIF.

            CLEAR lv_potext.
            LOOP AT po_text INTO lv_line.
              CONCATENATE lv_potext lv_line INTO lv_potext SEPARATED BY space.
            ENDLOOP.
          ENDIF.

          CLEAR item_text.

          CALL FUNCTION 'READ_TEXT'
            EXPORTING
              id                      = 'F01'
              language                = sy-langu
              name                    = v_thead2
              object                  = 'EKPO'
            TABLES
              lines                   = item_text
            EXCEPTIONS
              id                      = 1
              language                = 2
              name                    = 3
              not_found               = 4
              object                  = 5
              reference_check         = 6
              wrong_access_to_archive = 7
              OTHERS                  = 8.
          IF sy-subrc <> 0.
* Implement suitable error handling here
          ENDIF.

          CLEAR lv_itemtext.
          LOOP AT item_text INTO lv_line.
            CONCATENATE lv_itemtext lv_line INTO lv_itemtext SEPARATED BY space.
          ENDLOOP.

          CLEAR v_thead3.
          CLEAR deliv_text.
          v_thead3 = wa_hdr-ebeln.

          CALL FUNCTION 'READ_TEXT'
            EXPORTING
              id                      = 'F07'
              language                = sy-langu
              name                    = v_thead3
              object                  = 'EKKO'
            TABLES
              lines                   = deliv_text
            EXCEPTIONS
              id                      = 1
              language                = 2
              name                    = 3
              not_found               = 4
              object                  = 5
              reference_check         = 6
              wrong_access_to_archive = 7
              OTHERS                  = 8.
          IF sy-subrc <> 0.
          ENDIF.

          CLEAR lv_delivtext.
          READ TABLE deliv_text INTO lv_delivtext INDEX 1.

          IF lv_delivtext-tdline IS INITIAL.
            t_header-keterangan = 'After Invoice Receipt'.
          ELSE.
            t_header-keterangan = lv_delivtext-tdline.
          ENDIF.

          TYPES: BEGIN OF ty_esll,
                   extrow TYPE esll-extrow,
                   srvpos TYPE esll-srvpos,
                   ktext1 TYPE esll-ktext1,
                   menge  TYPE esll-menge,
                   meins  TYPE esll-meins,
                   tbtwr  TYPE esll-tbtwr,
                   packno TYPE ekbe-packno,
                   lfbnr  TYPE ekbe-lfbnr,
                   belnr  TYPE ekbe-belnr,
                 END OF ty_esll.

          DATA: lt_esll       TYPE TABLE OF ty_esll.
          DATA: lt_esll_2       TYPE TABLE OF ty_esll.
          DATA : lv_lfbnr TYPE ekbe-lfbnr.
          DATA : lv_belnr TYPE ekbe-belnr.

          IF data_table-pstyp EQ '9'.

            IF data_table-knttp EQ 'U'.

              SELECT packno, belnr
                FROM ekbe
                WHERE ebeln EQ @wa_hdr-ebeln
                AND bewtp EQ 'E'
                INTO TABLE @DATA(lv_ekbe).

              LOOP AT lv_ekbe INTO DATA(wa_ekbe).
                SELECT extrow, srvpos, ktext1, menge, meins, tbtwr, packno
                  FROM esll
                  WHERE packno EQ @wa_ekbe-packno
                  INTO TABLE @lt_esll_2.

                SELECT SINGLE lfbnr
                    INTO @lv_lfbnr
                    FROM ekbe
                    WHERE packno EQ @wa_ekbe-packno.

                SELECT SINGLE belnr
                    INTO @lv_belnr
                    FROM ekbe
                    WHERE packno EQ @wa_ekbe-packno.

                LOOP AT lt_esll_2 INTO DATA(wa_esll_2).
                  wa_esll_2-lfbnr = lv_lfbnr.
                  wa_esll_2-belnr = lv_belnr.
                  APPEND wa_esll_2 TO lt_esll.
                ENDLOOP.

              ENDLOOP.
              SORT lt_esll BY packno DESCENDING.
            ELSE.

              SELECT SINGLE sub_packno INTO @DATA(lv_sub_packno)
                FROM esll
                WHERE packno EQ @data_table-packno.

              SELECT extrow, srvpos, ktext1, menge, meins, tbtwr, packno
                FROM esll
                WHERE packno EQ @lv_sub_packno
                INTO TABLE @lt_esll.
            ENDIF.

            LOOP AT lt_esll INTO DATA(ls_esll).

              SHIFT ls_result-ebelp LEFT DELETING LEADING '0'.
              SHIFT ls_esll-srvpos LEFT DELETING LEADING '0'.
              SHIFT ls_esll-extrow LEFT DELETING LEADING '0'.

              CLEAR : ls_result, lv_lfbnr.

              IF data_table-knttp EQ 'U'.

                ADD 1 TO lv_idx.
                ls_result-ebelp = lv_idx.
                SHIFT ls_result-ebelp LEFT DELETING LEADING '0'.

                ls_result-item = |{ ls_esll-ktext1 } { ls_esll-lfbnr }|.
                ls_result-ebeln = |{ v_thead3 }|.

                SELECT SINGLE waers
                  INTO @DATA(lv_waers2)
                  FROM ekko
                  WHERE ebeln EQ @wa_hdr-ebeln.

                "Currency unit price
                WRITE ls_esll-tbtwr TO ls_result-unitprice CURRENCY lv_waers2 RIGHT-JUSTIFIED. "Rev 3

                "Currency amount
                lv_amount = ls_esll-tbtwr * ls_esll-menge.

                WRITE lv_amount TO ls_result-amount CURRENCY lv_waers2 RIGHT-JUSTIFIED.

                t_header-subtotal = t_header-subtotal + lv_amount.

              ELSE.
                IF sy-tabix = 1.
                  ADD 1 TO lv_idx.
                  ls_result-ebelp = lv_idx .
                  SHIFT ls_result-ebelp LEFT DELETING LEADING '0'.
                  ls_result-item = |{ data_table-txz01 } { lv_potext-tdline } { lv_itemtext-tdline }|.
                  APPEND ls_result TO t_result.
                  ls_result-ebelp = lv_idx .
                  SHIFT ls_result-ebelp LEFT DELETING LEADING '0'.
                  ls_result-item = |{ ls_esll-extrow } { ls_esll-srvpos } { ls_esll-ktext1 }|.
                ELSE.
                  ls_result-ebelp = lv_idx .
                  SHIFT ls_result-ebelp LEFT DELETING LEADING '0'.
                  ls_result-item = |{ ls_esll-extrow } { ls_esll-srvpos } { ls_esll-ktext1 }|.
                  ls_result-itemdate = | { lv_eindt+6(2) }.{ lv_eindt+4(2) }.{ lv_eindt(4) } |.
                ENDIF.
              ENDIF.

              SELECT SINGLE waers
                INTO @DATA(lv_waers)
                FROM ekko
                WHERE ebeln EQ @wa_hdr-ebeln.
              "---------------------------------------------------------------End Of Rev 3

              "---------------------------------------------------------------Rev 3
              "Format quantity
              CALL FUNCTION 'ROUND'
                EXPORTING
                  decimals      = 0
                  input         = ls_esll-menge
*                 SIGN          = ' '
                IMPORTING
                  output        = lv_menge
                EXCEPTIONS
                  input_invalid = 1
                  overflow      = 2
                  type_invalid  = 3
                  OTHERS        = 4.
              IF sy-subrc <> 0.
* Implement suitable error handling here
              ENDIF.

              WRITE lv_menge TO ls_result-menge UNIT ls_esll-meins RIGHT-JUSTIFIED.

              "Format satuan
              CALL FUNCTION 'CONVERSION_EXIT_CUNIT_OUTPUT'
                EXPORTING
                  input          = ls_esll-meins
*                 LANGUAGE       = SY-LANGU
                IMPORTING
*                 LONG_TEXT      =
                  output         = ls_result-meins
*                 SHORT_TEXT     =
                EXCEPTIONS
                  unit_not_found = 1
                  OTHERS         = 2.
              IF sy-subrc <> 0.
* Implement suitable error handling here
              ENDIF.
              "---------------------------------------------------------------End Of Rev 3

              SELECT SINGLE slocx
                INTO @DATA(lv_lgobe)
                FROM zsriloc_00001
                WHERE lgort EQ @data_table-lgort.

              CLEAR ls_result-sloctax.
              IF lv_lgobe IS NOT INITIAL AND data_table-mwskz IS NOT INITIAL.
                ls_result-sloctax = | { lv_lgobe }-{ data_table-mwskz } |.
              ELSEIF lv_lgobe IS NOT INITIAL AND data_table-mwskz IS INITIAL.
                ls_result-sloctax = lv_lgobe.
              ELSEIF lv_lgobe IS INITIAL AND data_table-mwskz IS NOT INITIAL.
                ls_result-sloctax = data_table-mwskz.
              ENDIF.
              CLEAR lv_lgobe.

              "Currency unit price
              WRITE ls_esll-tbtwr DECIMALS 2 TO ls_result-unitprice CURRENCY lv_waers RIGHT-JUSTIFIED. "Rev 3

              "Currency amount
              lv_amount = ls_esll-tbtwr * ls_esll-menge * 100.

              IF data_table-mwskz IS NOT INITIAL.
                SELECT SINGLE knumh INTO @DATA(lv_knumh3)
                  FROM a003
                  WHERE kappl EQ 'TX' AND kschl EQ 'MWVS' AND aland EQ 'ID' AND mwskz EQ @data_table-mwskz.

                SELECT SINGLE kbetr INTO @DATA(lv_kbetr3) FROM konp
                  WHERE knumh EQ @lv_knumh3.
              ENDIF.

              DATA : lv_ppn3 TYPE c LENGTH 20.
              DATA : lv_ppn5 TYPE p LENGTH 15 DECIMALS 2 ."DECIMALS 3.

              lv_ppn3 = lv_amount * lv_kbetr3 / 1000.

              DATA : lv_total2 TYPE c LENGTH 19,
                     num3      TYPE c LENGTH 15,
                     num4      TYPE c LENGTH 3.

              SHIFT lv_ppn3 LEFT DELETING LEADING ' '.

              SPLIT lv_ppn3 AT '.' INTO num3 num4.

              IF num4 < 999.
                lv_ppn3 = |{ num3 }|.
              ENDIF.

              PACK lv_ppn3 TO lv_ppn5.

              CLEAR t_header-total_ppn.
              t_header-total_ppn = t_header-total_ppn + lv_ppn5.
              t_header-total = ( t_header-total_ppn ) + t_header-subtotal.

              CLEAR lv_knumh3.
              CLEAR lv_kbetr3.

              WRITE lv_amount DECIMALS 2 TO ls_result-amount CURRENCY lv_waers RIGHT-JUSTIFIED.
              APPEND ls_result TO t_result.

              APPEND VALUE #( ebelp     = |{ ls_result-ebelp }|
              ebeln     = |{ v_thead3 }|
              item      = |{ ls_result-item }|
              menge     = |{ ls_result-menge }|
              meins     = |{ ls_result-meins }|
              unitprice = |{ ls_result-unitprice }|
              amount    = |{ ls_result-amount }|
              sloctax   = |{ ls_result-sloctax }|
              ) TO lt_item.

              lt_header-lifnr = |{ t_header-lifnr }|.
              lt_header-name1 = |{ t_header-name1 }|.
              lt_header-name2 = |{ t_header-name2 }|.
              lt_header-str_suppl2 = |{ t_header-str_suppl2 }|.
              lt_header-str_suppl1 = |{ t_header-str_suppl1 }|.
              lt_header-region = |{ t_header-region }|.
              lt_header-ebeln = |{ t_header-ebeln }|.
              lt_header-adrnr = |{ t_header-adrnr }|.
              lt_header-subtotal = |{ t_header-subtotal }|.
              WRITE t_header-subtotal TO lt_header-subtotal CURRENCY lv_waers RIGHT-JUSTIFIED.
              t_header-total_ppn = ( ( t_header-subtotal * 11 ) / 100 ) / 10.
              lt_header-total_ppn = |{ t_header-total_ppn }|.
              WRITE t_header-total_ppn TO lt_header-total_ppn CURRENCY lv_waers RIGHT-JUSTIFIED.
*      t_header-total = ( t_header-total_ppn  + t_header-subtotal ).
              t_header-total = ( t_header-total_ppn * 10 )  + t_header-subtotal .
              WRITE t_header-total TO lt_header-total CURRENCY lv_waers RIGHT-JUSTIFIED.
*      lt_header-total = |{ t_header-total }|.
              lt_header-zterm = |{ lv_top }|.
              lt_header-remarks = |{ t_header-remarks }|.
              lt_header-alamat = |{ t_header-alamat }|.
              lt_header-mc_city1 = |{ t_header-mc_city1 }|.
              lt_header-bezei = |{ t_header-bezei }|.
              lt_header-aedat = |{ t_header-aedat }|.
              lt_header-knumv = |{ t_header-knumv }|.
              lt_header-telfx = |{ t_header-telfx }|.
              lt_header-smtp_addr = |{ t_header-smtp_addr }|.
              lt_header-verkf = |{ t_header-verkf }|.
              lt_header-keterangan = |{ t_header-keterangan }|.
              lv_count = lv_idx.
            ENDLOOP.
          ELSE.

            IF data_table-mwskz IS NOT INITIAL.
              SELECT SINGLE knumh INTO @DATA(lv_knumh2)
                FROM a003
                WHERE kappl EQ 'TX' AND kschl EQ 'MWVS' AND aland EQ 'ID' AND mwskz EQ @data_table-mwskz.

              SELECT SINGLE kbetr INTO @DATA(lv_kbetr2) FROM konp
                WHERE knumh EQ @lv_knumh2.
            ENDIF.

            DATA : lv_ppn2 TYPE c LENGTH 20.
            DATA : lv_ppn4 TYPE p LENGTH 15 DECIMALS 2.

            lv_ppn2 = data_table-netwr * lv_kbetr2 / 10.

            DATA : lv_total TYPE c LENGTH 19,
                   num1     TYPE c LENGTH 15,
                   num2     TYPE c LENGTH 3.
*
            SHIFT lv_ppn2 LEFT DELETING LEADING ' '.
*
            SPLIT lv_ppn2 AT '.' INTO num1 num2.
*
            IF num2 < 999.
              lv_ppn2 = |{ num1 }|.
            ENDIF.

            PACK lv_ppn2 TO lv_ppn4.

            CLEAR t_header-total_ppn.
            t_header-total_ppn = ( t_header-total_ppn + lv_ppn4 ) / 10.

            CLEAR lv_knumh2.
            CLEAR lv_kbetr2.

            ADD 1 TO lv_idx.
            ls_result-ebelp = lv_idx .
            SHIFT ls_result-ebelp LEFT DELETING LEADING '0'.

*            ls_result-item = |{ data_table-matnr } { data_table-txz01 } { lv_potext-tdline } { lv_itemtext-tdline }{ cl_abap_char_utilities=>newline }{ lv_eindt+6(2) }.{ lv_eindt+4(2) }.{ lv_eindt(4) }{ cl_abap_char_utilities=>newline } |.
            ls_result-item = |{ data_table-txz01 } { lv_potext-tdline } { lv_itemtext-tdline }|.
            ls_result-itemdate = | { lv_eindt+6(2) }.{ lv_eindt+4(2) }.{ lv_eindt(4) } |.

            SELECT SINGLE waers
              INTO @lv_waers
              FROM ekko
              WHERE ebeln EQ @wa_hdr-ebeln.

            CALL FUNCTION 'ROUND'
              EXPORTING
                decimals      = 0
                input         = data_table-menge
*               SIGN          = ' '
              IMPORTING
                output        = lv_menge
              EXCEPTIONS
                input_invalid = 1
                overflow      = 2
                type_invalid  = 3
                OTHERS        = 4.
            IF sy-subrc <> 0.
* Implement suitable error handling here
            ENDIF.

            WRITE lv_menge TO ls_result-menge UNIT data_table-meins RIGHT-JUSTIFIED.

            "Format satuan
            CALL FUNCTION 'CONVERSION_EXIT_CUNIT_OUTPUT'
              EXPORTING
                input          = data_table-meins
*               LANGUAGE       = SY-LANGU
              IMPORTING
*               LONG_TEXT      =
                output         = ls_result-meins
*               SHORT_TEXT     =
              EXCEPTIONS
                unit_not_found = 1
                OTHERS         = 2.
            IF sy-subrc <> 0.
* Implement suitable error handling here
            ENDIF.

            SELECT SINGLE slocx
              INTO @lv_lgobe
              FROM zsriloc_00001
              WHERE lgort EQ @data_table-lgort.

            CLEAR ls_result-sloctax.
            IF lv_lgobe IS NOT INITIAL AND data_table-mwskz IS NOT INITIAL.
              ls_result-sloctax = | { lv_lgobe }-{ data_table-mwskz } |.
            ELSEIF lv_lgobe IS NOT INITIAL AND data_table-mwskz IS INITIAL.
              ls_result-sloctax = lv_lgobe.
            ELSEIF lv_lgobe IS INITIAL AND data_table-mwskz IS NOT INITIAL.
              ls_result-sloctax = data_table-mwskz.
            ENDIF.
            CLEAR lv_lgobe.
            CLEAR data_table-mwskz.

            "Currency unit price
            WRITE data_table-netpr TO ls_result-unitprice CURRENCY lv_waers RIGHT-JUSTIFIED. "Rev 3

            "Currency amount
            WRITE data_table-netwr TO ls_result-amount CURRENCY lv_waers RIGHT-JUSTIFIED. "Rev 3

            APPEND VALUE #( ebelp     = |{ ls_result-ebelp }|
                          item      = |{ ls_result-item }|
                          itemdate = |{ ls_result-itemdate }|
                          ebeln     = |{ v_thead3 }|
                          menge     = |{ ls_result-menge }|
                          meins     = |{ ls_result-meins }|
                          unitprice = |{ ls_result-unitprice }|
                          amount    = |{ ls_result-amount }|
                          sloctax   = |{ ls_result-sloctax }|
          )
          TO lt_item.


            lt_header-lifnr = |{ t_header-lifnr }|.
            lt_header-name1 = |{ t_header-name1 }|.
            lt_header-name2 = |{ t_header-name2 }|.
            lt_header-str_suppl2 = |{ t_header-str_suppl2 }|.
            lt_header-str_suppl1 = |{ t_header-str_suppl1 }|.
            lt_header-region = |{ t_header-region }|.
            lt_header-ebeln = |{ t_header-ebeln }|.
            lt_header-adrnr = |{ t_header-adrnr }|.
            lt_header-subtotal = |{ t_header-subtotal }|.
            WRITE t_header-subtotal TO lt_header-subtotal CURRENCY lv_waers RIGHT-JUSTIFIED.
            t_header-total_ppn = ( ( t_header-subtotal  * 11 ) / 100 ) / 10.
            WRITE t_header-total_ppn TO lt_header-total_ppn CURRENCY lv_waers RIGHT-JUSTIFIED.
            t_header-total = ( t_header-total_ppn * 10 )  + t_header-subtotal .
            WRITE t_header-total TO lt_header-total CURRENCY lv_waers RIGHT-JUSTIFIED.
            lt_header-zterm = |{ lv_top }|.
            lt_header-remarks = |{ t_header-remarks }|.
            lt_header-alamat = |{ t_header-alamat }|.
            lt_header-mc_city1 = |{ t_header-mc_city1 }|.
            lt_header-bezei = |{ t_header-bezei }|.
            lt_header-aedat = |{ t_header-aedat }|.
            lt_header-knumv = |{ t_header-knumv }|.
            lt_header-telfx = |{ t_header-telfx }|.
            lt_header-smtp_addr = |{ t_header-smtp_addr }|.
            lt_header-verkf = |{ t_header-verkf }|.
            lt_header-keterangan = |{ t_header-keterangan }|.
            lv_count = lv_idx.

          ENDIF.
          CONDENSE ls_result-menge.
        ENDLOOP.

        lv_count_str = |{ lv_count }|.

        WRITE t_header-subtotal TO lt_header-subtotal CURRENCY lv_waers RIGHT-JUSTIFIED.
        WRITE t_header-total_ppn TO lt_header-total_ppn CURRENCY lv_waers RIGHT-JUSTIFIED.

        APPEND VALUE #(
          ebeln = |{ wa_hdr-ebeln }|
          aedat = |{ wa_hdr-aedat }|
          name1 = |{ t_header-name1 }|
          alamat = |{ t_header-alamat }|
          str_suppl1 = |{ t_header-str_suppl1 }|
          mc_city1 = |{ t_header-mc_city1 }|
          bezei = |{ t_header-bezei }|
          verkf = |{ t_header-verkf }|
          telfx = |{ t_header-telfx }|
          smtp_addr = |{ t_header-smtp_addr }|
          zterm = |{ lv_top }|
          subtotal = |{ lt_header-subtotal }|
          total_ppn = |{ lt_header-total_ppn }|
          total = |{ lt_header-total }|
          keterangan = |{ t_header-keterangan }|
          sfpage = lv_count_str
        )
        TO lp_header.

        CLEAR lt_header-total_ppn.
        CLEAR lt_header-total.
        CLEAR lt_header-subtotal.

      ENDIF.
    ENDLOOP.

* Get the function module name using form name
    CALL FUNCTION 'SSF_FUNCTION_MODULE_NAME'
      EXPORTING
        formname           = gv_formname                 " Form name
      IMPORTING
        fm_name            = gv_fm_name
      EXCEPTIONS
        no_form            = 1
        no_function_module = 2
        OTHERS             = 3.
    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
        WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.

* Get device type
    CALL FUNCTION 'SSF_GET_DEVICE_TYPE'
      EXPORTING
        i_language             = sy-langu                 " Smart Forms: Form Language
      IMPORTING
        e_devtype              = gv_devtype                 " Spool: Device type name
      EXCEPTIONS
        no_language            = 1
        language_not_installed = 2
        no_devtype_found       = 3
        system_error           = 4                " Error determining installed languages
        OTHERS                 = 5.
    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
        WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.

    gwa_ssfcompop-tdprinter = gv_devtype.
    gwa_control-no_dialog = 'X'.
    gwa_control-getotf = 'X'.

    DATA: v_lin  TYPE i,
          v_ind  TYPE i,
          v_item TYPE i.

    DESCRIBE TABLE lp_header LINES v_lin.
    READ TABLE lp_header INTO DATA(wa_headers) INDEX v_lin.
    APPEND wa_headers TO gt_header.

    DESCRIBE TABLE lt_item LINES v_item.
*    BREAK-POINT.
    DO v_item TIMES.

      v_ind = v_ind + 1.

      READ TABLE lt_item INTO DATA(wa_items) INDEX v_ind.
      APPEND wa_items TO gt_item.

      CLEAR : wa_items.

    ENDDO.

    CLEAR : lt_item, v_ind.

* Trigger the smartform
    CALL FUNCTION gv_fm_name
      EXPORTING
        control_parameters = gwa_control
        output_options     = gwa_ssfcompop
        data_header        = lt_header
*       data_table         = lt_item
        data_table         = gt_item
*       header_loop        = lp_header
        header_loop        = gt_header
      IMPORTING
        job_output_info    = gv_job_output
      EXCEPTIONS
        formatting_error   = 1
        internal_error     = 2
        send_error         = 3
        user_canceled      = 4
        OTHERS             = 5.
    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
        WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.

*  Convert OTF to PDF
    CALL FUNCTION 'CONVERT_OTF'
      EXPORTING
        format                = 'PDF'          " Target format for LINES table
      IMPORTING
        bin_filesize          = gv_size                  " For binary format: Number of bytes in LINES
*       bin_file              =
      TABLES
        otf                   = gv_job_output-otfdata                 " Input table with OTF format
        lines                 = gt_lines                 " Output table with target format
      EXCEPTIONS
        err_max_linewidth     = 1                " Line width must be between 2 and 132
        err_format            = 2                " Format not supported
        err_conv_not_possible = 3                " Conversion not possible/supported
        err_bad_otf           = 4
        OTHERS                = 5.
    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
        WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.

    filename = |{ '\\server-data\it-doc$\Development\Suryadi\PO\' }{ wa_pdf-ebeln }.pdf|.

*  Download PDF file to presentation server
    CALL FUNCTION 'GUI_DOWNLOAD'
      EXPORTING
        bin_filesize            = gv_size                                                 " File length for binary files
        filename                = filename                     " Name of file
        filetype                = 'BIN'                " File Type (ASC or BIN)
      TABLES
        data_tab                = gt_lines                    " Transfer table
      EXCEPTIONS
        file_write_error        = 1                    " Cannot write to file
        no_batch                = 2                    " Front-End Function Cannot Be Executed in Background
        gui_refuse_filetransfer = 3                    " Incorrect Front End
        invalid_type            = 4                    " Invalid value for parameter FILETYPE
        no_authority            = 5                    " No Download Authorization
        unknown_error           = 6
        header_not_allowed      = 7                    " Invalid header
        separator_not_allowed   = 8                    " Invalid separator
        filesize_not_allowed    = 9                    " Invalid file size
        header_too_long         = 10                   " The header information is limited to 1023 bytes at present
        dp_error_create         = 11                   " Cannot Create Data Provider
        dp_error_send           = 12                   " Error Sending Data with DataProvider
        dp_error_write          = 13                   " Error Writing Data with DataProvider
        unknown_dp_error        = 14                   " Error when calling data provider
        access_denied           = 15                   " Access to File Denied
        dp_out_of_memory        = 16                   " Not Enough Memory in DataProvider
        disk_full               = 17                   " Storage Medium Full
        dp_timeout              = 18                   " Timeout of DataProvider
        file_not_found          = 19                   " Could not find file
        dataprovider_exception  = 20                   " General Exception Error in Data Provider
        control_flush_error     = 21                   " Error in Control Framework
        OTHERS                  = 22.
    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
        WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.

*    BREAK-POINT.

*    CLEAR : t_header, lt_header, ls_result, gv_job_output, gt_lines, gv_size, gwa_control, gwa_ssfcompop, data_table, wa_hdr,
    CLEAR : gt_header, gt_item .

  ENDLOOP.

ENDFORM.

FORM call_form.

* Get the function module name using form name
  CALL FUNCTION 'SSF_FUNCTION_MODULE_NAME'
    EXPORTING
      formname           = gv_formname                 " Form name
    IMPORTING
      fm_name            = gv_fm_name
    EXCEPTIONS
      no_form            = 1
      no_function_module = 2
      OTHERS             = 3.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
      WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

* Get device type
  CALL FUNCTION 'SSF_GET_DEVICE_TYPE'
    EXPORTING
      i_language             = sy-langu                 " Smart Forms: Form Language
    IMPORTING
      e_devtype              = gv_devtype                 " Spool: Device type name
    EXCEPTIONS
      no_language            = 1
      language_not_installed = 2
      no_devtype_found       = 3
      system_error           = 4                " Error determining installed languages
      OTHERS                 = 5.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
      WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

  gwa_ssfcompop-tdprinter = gv_devtype.
  gwa_control-no_dialog = 'X'.
  gwa_control-getotf = 'X'.

  DATA: v_ind TYPE i,
        v_lin TYPE i.

* Trigger the smartform
  CALL FUNCTION gv_fm_name
    EXPORTING
      control_parameters = gwa_control
      output_options     = gwa_ssfcompop
      data_header        = lt_header
      data_table         = lt_item
      header_loop        = lp_header
    IMPORTING
      job_output_info    = gv_job_output
    EXCEPTIONS
      formatting_error   = 1
      internal_error     = 2
      send_error         = 3
      user_canceled      = 4
      OTHERS             = 5.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
      WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

*  Convert OTF to PDF
  CALL FUNCTION 'CONVERT_OTF'
    EXPORTING
      format                = 'PDF'          " Target format for LINES table
    IMPORTING
      bin_filesize          = gv_size                  " For binary format: Number of bytes in LINES
*     bin_file              =
    TABLES
      otf                   = gv_job_output-otfdata                 " Input table with OTF format
      lines                 = gt_lines                 " Output table with target format
    EXCEPTIONS
      err_max_linewidth     = 1                " Line width must be between 2 and 132
      err_format            = 2                " Format not supported
      err_conv_not_possible = 3                " Conversion not possible/supported
      err_bad_otf           = 4
      OTHERS                = 5.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
      WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

  filename = |{ '\\server-data\it-doc$\Development\Suryadi\PO\' }{ wa_pdf-ebeln }.pdf|.

*  Download PDF file to presentation server
  CALL FUNCTION 'GUI_DOWNLOAD'
    EXPORTING
      bin_filesize            = gv_size                                                 " File length for binary files
      filename                = filename                     " Name of file
      filetype                = 'BIN'                " File Type (ASC or BIN)
    TABLES
      data_tab                = gt_lines                    " Transfer table
    EXCEPTIONS
      file_write_error        = 1                    " Cannot write to file
      no_batch                = 2                    " Front-End Function Cannot Be Executed in Background
      gui_refuse_filetransfer = 3                    " Incorrect Front End
      invalid_type            = 4                    " Invalid value for parameter FILETYPE
      no_authority            = 5                    " No Download Authorization
      unknown_error           = 6
      header_not_allowed      = 7                    " Invalid header
      separator_not_allowed   = 8                    " Invalid separator
      filesize_not_allowed    = 9                    " Invalid file size
      header_too_long         = 10                   " The header information is limited to 1023 bytes at present
      dp_error_create         = 11                   " Cannot Create Data Provider
      dp_error_send           = 12                   " Error Sending Data with DataProvider
      dp_error_write          = 13                   " Error Writing Data with DataProvider
      unknown_dp_error        = 14                   " Error when calling data provider
      access_denied           = 15                   " Access to File Denied
      dp_out_of_memory        = 16                   " Not Enough Memory in DataProvider
      disk_full               = 17                   " Storage Medium Full
      dp_timeout              = 18                   " Timeout of DataProvider
      file_not_found          = 19                   " Could not find file
      dataprovider_exception  = 20                   " General Exception Error in Data Provider
      control_flush_error     = 21                   " Error in Control Framework
      OTHERS                  = 22.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
      WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

ENDFORM.

FORM f_display_setting .

  DATA: lo_display TYPE REF TO cl_salv_display_settings.

  lo_display = o_alv->get_display_settings( ).
  lo_display->set_striped_pattern( 'X' ).
  lo_display->set_list_header( 'Smartform to PDF and Email' ).

ENDFORM.

FORM f_set_status USING rt_extab TYPE slis_t_extab.

*  o_alv->set_screen_status(
*  pfstatus      =  'STANDARD_STATUS'
*  report        =  sy-repid
*  set_functions = o_alv->c_functions_all ).

  DESCRIBE TABLE rt_extab.
  SET PF-STATUS 'STANDARD_STATUS'.
*  SET PF-STATUS 'SALV_REPORTS'.

ENDFORM.

FORM build_f_catalog.

  fieldcatalog-fieldname = 'EBELN'.
  fieldcatalog-seltext_M = 'Purchase Order'.
  fieldcatalog-col_pos     = 0.
  APPEND fieldcatalog TO fieldcatalog.
  fieldcatalog-fieldname = 'LIFNR'.
  fieldcatalog-seltext_M = 'Kode Supplier'.
  fieldcatalog-col_pos     = 1.
  APPEND fieldcatalog TO fieldcatalog.
  fieldcatalog-fieldname = 'NAME1'.
  fieldcatalog-seltext_M = 'Supplier'.
  fieldcatalog-col_pos     = 2.
  APPEND fieldcatalog TO fieldcatalog.
**  CLEAR fieldcatalog.
*  fieldcatalog-fieldname = 'REVNO'.
*  fieldcatalog-seltext_M = 'Version'.
*  fieldcatalog-col_pos     = 3.
*  APPEND fieldcatalog TO fieldcatalog.
*  CLEAR fieldcatalog.
  fieldcatalog-fieldname = 'AEDAT'.
  fieldcatalog-seltext_M = 'Creation Date'.
  fieldcatalog-col_pos     = 3.
  APPEND fieldcatalog TO fieldcatalog.
  CLEAR fieldcatalog.
  fieldcatalog-fieldname = 'EKGRP'.
  fieldcatalog-seltext_M = 'Purchasing Group'.
  fieldcatalog-col_pos     = 4.
  APPEND fieldcatalog TO fieldcatalog.
  fieldcatalog-fieldname = 'FRGKE'.
  fieldcatalog-seltext_M = 'PO Release'.
  fieldcatalog-col_pos     = 5.
  APPEND fieldcatalog TO fieldcatalog.
  CLEAR fieldcatalog.
  fieldcatalog-fieldname = 'WAERS'.
  fieldcatalog-seltext_M = 'Currency'.
  fieldcatalog-col_pos     = 6.
  APPEND fieldcatalog TO fieldcatalog.
  CLEAR fieldcatalog.
  fieldcatalog-fieldname = 'STATUS'.
  fieldcatalog-seltext_M = 'Status'.
  fieldcatalog-col_pos     = 7.
  APPEND fieldcatalog TO fieldcatalog.
  CLEAR fieldcatalog.
  fieldcatalog-fieldname = 'EMAIL'.
  fieldcatalog-seltext_M = 'Email'.
  fieldcatalog-col_pos     = 8.
  APPEND fieldcatalog TO fieldcatalog.
  CLEAR fieldcatalog.
  fieldcatalog-fieldname = 'ICON'.
  fieldcatalog-seltext_M = 'Icon'.
  fieldcatalog-col_pos     = 9.
  APPEND fieldcatalog TO fieldcatalog.
  CLEAR fieldcatalog.

ENDFORM.

FORM f_display_data.

  DATA: lx_msg TYPE REF TO cx_salv_msg.
  DATA: lo_display TYPE REF TO cl_salv_display_settings.
  DATA: t_slis_layout_alv TYPE slis_layout_alv.

  t_slis_layout_alv-colwidth_optimize = 'X'.

  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
    EXPORTING
*     i_interface_check        = space            " Interface consistency check log output
*     i_bypassing_buffer       = space            " Ignore all buffers
*     i_buffer_active          = space            " Buffering active
      i_callback_program       = sy-repid            " Name of the calling program
      i_callback_pf_status_set = 'F_SET_STATUS'            " Set EXIT routine to status
      i_callback_user_command  = 'USER_COMMAND'            " EXIT routine for command handling
*     i_callback_top_of_page   = space            " EXIT routine for handling TOP-OF-PAGE
*     i_callback_html_top_of_page = space            " EXIT routine for HTML TOP-OF-PAGE
*     i_callback_html_end_of_list = space            " EXIT routine for HTML END-OF-LIST
*     i_structure_name         =                  " Internal output table structure name
*     i_background_id          =                  " Object ID of wallpaper
*     i_grid_title             =                  " Control title
*     i_grid_settings          =                  " Grid settings
      is_layout                = t_slis_layout_alv                " List layout specifications
      it_fieldcat              = fieldcatalog[]                  " Field catalog with field descriptions
*     it_excluding             =                  " Table of inactive function codes
*     it_special_groups        =                  " Grouping fields for column selection
*     it_sort                  =                  " Sort criteria for first list display
*     it_filter                =                  " Filter criteria for first list output
*     is_sel_hide              =                  " Selection information modification
*     i_default                = 'X'              " Initial variant active/inactive logic
*     i_save                   = space            " Variants can be saved
*     is_variant               =                  " Variant information
*     it_events                =                  " Table of events to perform
*     it_event_exit            =                  " Standard fcode exit requests table
*     is_print                 =                  " Print information
*     is_reprep_id             =                  " Initialization key for Re/Re interface
*     i_screen_start_column    = 0                " Coordinates for list in dialog box
*     i_screen_start_line      = 0                " Coordinates for list in dialog box
*     i_screen_end_column      = 0                " Coordinates for list in dialog box
*     i_screen_end_line        = 0                " Coordinates for list in dialog box
*     i_html_height_top        = 0                " HTML_TOP_OF_PAGE Height
*     i_html_height_end        = 0                " HTML_END_OF_PAGE Height
*     it_alv_graphics          =                  " Parameter for ALV graphic
*     it_hyperlink             =                  " Hyperlinks
*     it_add_fieldcat          =                  " Additional Field Catalog Options
*     it_except_qinfo          =
*     ir_salv_fullscreen_adapter  =                  " Adapter Fullscreen -> Table
*     o_previous_sral_handler  =
*    IMPORTING
*     e_exit_caused_by_caller  =                  " Delete list in CALLBACK_USER_COMMAND
*     es_exit_caused_by_user   =                  " How the user left the list
    TABLES
      t_outtab                 = it_data                 " Table with data to be displayed
    EXCEPTIONS
      program_error            = 1                " Program errors
      OTHERS                   = 2.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
      WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

ENDFORM.

FORM user_command USING r_ucomm LIKE sy-ucomm
                        rs_selfield TYPE slis_selfield.

  CASE r_ucomm.
    WHEN '%PDF'.
      PERFORM f_data_smartform.
      PERFORM validation.
    WHEN OTHERS.
  ENDCASE.

ENDFORM.

FORM report_excel.

  DATA: lv_date TYPE c LENGTH 11.

  DATA:
    lo_excel     TYPE REF TO zcl_excel,
    lo_worksheet TYPE REF TO zcl_excel_worksheet,
    lo_style_1   TYPE REF TO zcl_excel_style,
    lo_style_2   TYPE REF TO zcl_excel_style.

  DATA: gr_table     TYPE REF TO cl_salv_table,
        lr_functions TYPE REF TO cl_salv_functions_list,
        lr_layout    TYPE REF TO cl_salv_layout,         " Settings for Layout
        lv_xml_type  TYPE salv_bs_constant,
        lv_xml       TYPE xstring,
        gt_len       TYPE i,                             " Len of type Integers
        lv_file_xlsx TYPE string,
        lv_file      TYPE localfile,                     " Local file for upload/download
        ls_key       TYPE salv_s_layout_key,             " Layout Key
        lr_content   TYPE REF TO cl_salv_form_element,
        lt_excel     TYPE w3mimetabtype.

  DATA: numbytes   TYPE i,
        subtotal   TYPE i,
        grandtotal TYPE i,
        old_val    TYPE c LENGTH 30.

  LOOP AT it_data INTO DATA(wa_excel).

    SELECT a~ebeln, a~lifnr, b~matnr, b~txz01, b~ebelp, b~meins, c~eindt, SUM( b~menge ) AS menge
      INTO TABLE @DATA(it_ret) FROM ekko AS a
      LEFT JOIN ekpo AS b
      ON a~ebeln EQ b~ebeln
      LEFT JOIN eket AS c
      ON a~ebeln EQ c~ebeln AND
      b~ebelp EQ c~ebelp
      WHERE a~ebeln EQ @wa_excel-ebeln
      GROUP BY a~ebeln, a~lifnr,  b~matnr, b~txz01, b~ebelp, b~meins, c~eindt, b~menge, b~ebelp.

    SORT it_ret BY matnr eindt ASCENDING.

    LOOP AT it_ret INTO DATA(wa_ret).

      MOVE-CORRESPONDING wa_ret TO wa_datas.

      wa_datas-no = wa_datas-no + 1.

      SELECT SINGLE name1 INTO @wa_datas-name1 FROM lfa1
        WHERE lifnr = @wa_ret-lifnr.

      SELECT SINGLE name1 INTO @DATA(wa_name1)
      FROM lfa1
      WHERE lifnr = @wa_ret-lifnr.

      SELECT SINGLE eindt INTO @DATA(wa_date)
        FROM eket
        WHERE ebeln = @wa_ret-ebeln
        AND ebelp = @wa_ret-ebelp.

      SELECT SINGLE ktx FROM t247 INTO @DATA(lv_ktx) WHERE mnr = @wa_date+4(2)
        AND spras EQ 'E'.

      wa_datas-eindt = |{ wa_date+6(2) }-{ lv_ktx }-{ wa_date(4) }|.

      grandtotal = grandtotal + wa_datas-menge.

      APPEND wa_datas TO lt_datas.

    ENDLOOP.

    READ TABLE lt_datas INTO DATA(work_area1) INDEX 1.

    IF sy-subrc IS INITIAL.
      old_val = work_area1-matnr.
    ENDIF.

    LOOP AT lt_datas INTO DATA(wa_data3).
      IF wa_data3-matnr <> old_val.
        new_line-no = ''.
        new_line-txz01 = 'Subtotal'.
        new_line-menge = subtotal.
        INSERT new_line INTO lt_datas INDEX sy-tabix.
        CLEAR subtotal.
        old_val = wa_data3-matnr.
      ENDIF.

      subtotal = subtotal + wa_data3-menge.
    ENDLOOP.

    DATA: v_lin TYPE i.

    DESCRIBE TABLE lt_datas LINES v_lin.
    v_lin = v_lin + 1.

    READ TABLE lt_datas INTO DATA(wa_data4) INDEX v_lin.

    new_line-no = ''.
    new_line-txz01 = 'Subtotal'.
    new_line-menge = subtotal.
    INSERT new_line INTO lt_datas INDEX v_lin.

    DESCRIBE TABLE lt_datas LINES v_lin.
    v_lin = v_lin + 1.

    READ TABLE lt_datas INTO DATA(wa_data2) INDEX v_lin.

    new_line-no = ''.
    new_line-txz01 = 'Grandtotal'.
    new_line-menge = grandtotal.
    INSERT new_line INTO lt_datas INDEX v_lin.

    TRY.

        cl_salv_table=>factory(
          IMPORTING
            r_salv_table = gr_table
          CHANGING
            t_table      = lt_datas ).

      CATCH cx_salv_msg.                                "#EC NO_HANDLER

    ENDTRY.

    lr_functions = gr_table->get_functions( ).
    lr_functions->set_all( abap_true ).
    lr_layout = gr_table->get_layout( ).
    ls_key-report = sy-repid.
    lr_layout->set_key( ls_key ).
    lr_layout->set_save_restriction( if_salv_c_layout=>restrict_user_independant ).
    gr_table->set_top_of_list( lr_content ).
    gr_table->set_end_of_list( lr_content ).

    lv_xml_type =  if_salv_bs_xml=>c_type_xlsx. "if_salv_bs_xml=>c_type_mhtml.
    lv_xml      = gr_table->to_xml( xml_type = lv_xml_type ).

    CALL FUNCTION 'SCMS_XSTRING_TO_BINARY'
      EXPORTING
        buffer        = lv_xml
      IMPORTING
        output_length = gt_len
      TABLES
        binary_tab    = lt_excel.

    filename = |{ '\\server-file\UPLOAD_SAP\PO\' }Delivery_Schedule-{ wa_ret-ebeln }.xlsx|.
*    filename = |{ 'C:\Users\dio.mahardhika\Downloads\' }R-{ wa_excel-ebeln }.xlsx|.

    CALL FUNCTION 'GUI_DOWNLOAD'
      EXPORTING
        bin_filesize            = gt_len
        filename                = filename
        filetype                = 'BIN'
*       confirm_overwrite       = 'X'
      IMPORTING
        filelength              = numbytes
      TABLES
        data_tab                = lt_excel
      EXCEPTIONS
        file_write_error        = 1
        no_batch                = 2
        gui_refuse_filetransfer = 3
        invalid_type            = 4
        no_authority            = 5
        unknown_error           = 6
        header_not_allowed      = 7
        separator_not_allowed   = 8
        filesize_not_allowed    = 9
        header_too_long         = 10
        dp_error_create         = 11
        dp_error_send           = 12
        dp_error_write          = 13
        unknown_dp_error        = 14
        access_denied           = 15
        dp_out_of_memory        = 16
        disk_full               = 17
        dp_timeout              = 18
        file_not_found          = 19
        dataprovider_exception  = 20
        control_flush_error     = 21
        OTHERS                  = 22.

    CLEAR: lt_datas, new_line, grandtotal, subtotal, v_lin, wa_datas.

  ENDLOOP.

ENDFORM.

FORM send_mail.

  DATA: lv_sender         TYPE uname,
        lv_recipient_mail TYPE ad_smtpadr,
        lv_cc_mail        TYPE ad_smtpadr,
        wa_to             TYPE ad_smtpadr,
        wa_cc             TYPE ad_smtpadr,
*        lv_subject        TYPE string.
        lv_message        TYPE string,
        p_pdf             TYPE file_table-filename, "VALUE 'C:\Users\dio.mahardhika\Downloads\PDFPO.pdf'.
        p_pdf_report      TYPE file_table-filename. "VALUE 'C:\Users\dio.mahardhika\Downloads\PDFPO.pdf'.

  DATA: lv_rc     TYPE i,
        it_files  TYPE filetable,
        lv_action TYPE i,
        retcode   TYPE soi_ret_string.

  DATA: lv_pdf_size     TYPE i,
        it_pdf_data_tab TYPE solix_tab,
        l_file          TYPE rlgrap-filename,
        numbytes        TYPE i.

  DATA: lv_lifnr TYPE c LENGTH 10.

  LOOP AT it_data INTO DATA(wa_mail).

    IF wa_mail-frgke = 'In Process'.
      MESSAGE 'Some document not completed' TYPE 'E'.
    ENDIF.

    lv_lifnr = |0000{ wa_mail-lifnr }|.

    p_pdf = |{ '\\server-file\UPLOAD_SAP\PO\' }{ wa_mail-ebeln }.pdf|.
    p_pdf_report = |{ '\\server-file\UPLOAD_SAP\PO\' }Delivery_Schedule-{ wa_mail-ebeln }.xlsx|.

    SELECT SINGLE addrcomm INTO @DATA(wa_but) FROM but000
    WHERE partner = @lv_lifnr.

    " List of 'TO' Email
*    SELECT smtp_addr INTO TABLE @DATA(it_to) FROM adr6
*      WHERE addrnumber = @wa_but
*      AND flg_nouse = ''.

    SELECT email_addr INTO TABLE @DATA(it_to) FROM zsrifdt_00024
      WHERE email_dest_type EQ 1
      AND lifnr EQ @lv_lifnr
      AND is_active EQ 'Y'.

    IF it_to IS INITIAL.
      MESSAGE 'Email failed to send !' TYPE 'E' DISPLAY LIKE 'E'.
    ENDIF.

    " List of 'CC' Email
*    SELECT smtp_addr INTO TABLE @DATA(it_cc) FROM adr6
*      WHERE addrnumber = @wa_but
*      AND flg_nouse = 'X'.

    SELECT email_addr INTO TABLE @DATA(it_cc) FROM zsrifdt_00024
      WHERE email_dest_type EQ 2
      AND lifnr EQ @lv_lifnr
      AND is_active EQ 'Y'.

    " Find region / country
    SELECT SINGLE land1 INTO @DATA(wa_region)
    FROM lfa1
    WHERE lifnr EQ @lv_lifnr.

    TRY.
        lv_recipient_mail = 'diomahardhika.nrp@gmail.com'.

        DATA: lv_subject TYPE sood-objdes VALUE 'Form Purchase Order'.

        o_send_request = cl_bcs=>create_persistent( ).

        IF wa_region = 'ID'.
          PERFORM f_create_body_email.
        ELSE.
          PERFORM f_create_body_email_english.
        ENDIF.

* create document (email)
        o_document = cl_document_bcs=>create_document(
        EXPORTING
            i_type    = 'HTM'
            i_text    = gt_text
            i_subject = lv_subject ).

*  add attachment PO
* GUI Upload
        cl_gui_frontend_services=>gui_upload( EXPORTING
                                              filename = |{ p_pdf }|
                                              filetype = 'BIN'
                                              IMPORTING
                                                 filelength = lv_pdf_size
                                              CHANGING
                                                 data_tab = it_pdf_data_tab ).

        CALL FUNCTION 'TRINT_SPLIT_FILE_AND_PATH'
          EXPORTING
            full_name     = p_pdf
          IMPORTING
            stripped_name = p_pdf.

        o_document->add_attachment( i_attachment_type = 'EXT'
                                    i_attachment_subject = |{ p_pdf }|
                                    i_att_content_hex = it_pdf_data_tab
                                    ).
        IF sy-subrc NE 0.
          MESSAGE 'File does not exist, please save the document first ! ' TYPE 'S' DISPLAY LIKE 'E'.
        ENDIF.

        IF p_report EQ 'X'.
* GUI Upload Report
          CLEAR: it_pdf_data_tab, lv_pdf_size.
          cl_gui_frontend_services=>gui_upload( EXPORTING
                                                filename = |{ p_pdf_report }|
                                                filetype = 'BIN'
                                                IMPORTING
                                                   filelength = lv_pdf_size
                                                CHANGING
                                                   data_tab = it_pdf_data_tab ).
*  add attachment report
          IF p_report EQ 'X'.
            CALL FUNCTION 'TRINT_SPLIT_FILE_AND_PATH'
              EXPORTING
                full_name     = p_pdf_report
              IMPORTING
                stripped_name = p_pdf_report.

            o_document->add_attachment( i_attachment_type = 'EXT'
                                      i_attachment_subject = |{ p_pdf_report }|
                                      i_att_content_hex = it_pdf_data_tab
                                      ).
          ENDIF.
        ENDIF.

*  create send request
        o_send_request = cl_bcs=>create_persistent( ).

*  o_send_request->set_message_subject( ip_subject = lv_subject ).
        o_send_request->set_document( o_document ).

*  SAP User as Sender
*        o_sender = cl_sapuser_bcs=>create( lv_sender ).
        lr_sender = cl_cam_address_bcs=>create_internet_address( 'no-reply@sri-astra.com' ).
        o_send_request->set_sender( lr_sender ).

*  Set Recipient
*        o_recipient = cl_cam_address_bcs=>create_internet_address( lv_recipient_mail ).
        o_send_request->add_recipient( i_recipient = o_recipient
                                       i_express = abap_true ).

        LOOP AT it_to INTO wa_to.

*          CLEAR o_recipient.
          o_recipient = cl_cam_address_bcs=>create_internet_address( wa_to ).
          o_send_request->add_recipient( i_recipient = o_recipient
                                         i_express = abap_true ).
        ENDLOOP.

* Set CC
        LOOP AT it_cc INTO wa_cc.

*          CLEAR cc_recipient.
          cc_recipient = cl_cam_address_bcs=>create_internet_address( wa_cc  ).
          o_send_request->add_recipient( i_recipient = cc_recipient
                                         i_express = abap_true
                                         i_copy = abap_true ).
        ENDLOOP.

* Send Email
        o_send_request->set_send_immediately( 'X' ).

*  Send Document (Email)
        o_send_request->send( i_with_error_screen = abap_true ).

        COMMIT WORK.

        IF wa_to IS NOT INITIAL .

          zsrift_0001-ebeln = wa_mail-ebeln.
          zsrift_0001-lifnr = wa_mail-lifnr.
          zsrift_0001-revno = wa_mail-revno.
          zsrift_0001-aedat = sy-datum.
          zsrift_0001-ernam = sy-uname.
          zsrift_0001-ekgrp = wa_mail-ekgrp.
          zsrift_0001-waers = wa_mail-waers.
          INSERT zsrift_0001.

          MESSAGE 'PO Terkirim !' TYPE 'S' DISPLAY LIKE 'S'.

        ENDIF.
*      ENDIF.

      CATCH cx_root INTO DATA(e_text).
        WRITE: / e_text->get_text( ).
    ENDTRY.

*    BREAK-POINT.

  ENDLOOP.

ENDFORM.

FORM f_create_body_email.

  DATA: ls_text TYPE LINE OF bcsy_text.

  ls_text-line = '<HTML><BODY>'.
  APPEND ls_text TO gt_text.
  ls_text-line = 'Dear Supplier,'.
  APPEND ls_text TO gt_text.
  ls_text-line = '<br><br></br>'.
  APPEND ls_text TO gt_text.
  ls_text-line = 'Berikut kami lampirkan Purchase Order & delivery Schedule :'.
  APPEND ls_text TO gt_text.
  ls_text-line = '<br><br></br>'.
  APPEND ls_text TO gt_text.
  ls_text-line = '* Mohon agar pengiriman barang / penyelesaian jasa dilakukan sesuai jadwal.'.
  APPEND ls_text TO gt_text.
  ls_text-line = '<br></br>'.
  APPEND ls_text TO gt_text.
  ls_text-line = '* Delivery Schedule tercantum pada masing masing item.'.
  APPEND ls_text TO gt_text.
  ls_text-line = '<br></br>'.
  APPEND ls_text TO gt_text.
  ls_text-line = '* Jika ada potensi keterlambatan pengiriman barang /penyelesaian jasa mohon menginformasikan ke PIC Purchase PT Suryaraya Rubberindo Industries.'.
  APPEND ls_text TO gt_text.
  ls_text-line = '<br></br>'.
  APPEND ls_text TO gt_text.
  ls_text-line = '* Invoice dan faktur pajak hanya boleh dibuat setelah barang / BAST jasa diterima oleh gudang PT Suryaraya Rubberindo Industries (mendapatkan bukti receipt / bukti penerimaan barang).'.
  APPEND ls_text TO gt_text.
  ls_text-line = '<br><br></br>'.
  APPEND ls_text TO gt_text.
  ls_text-line = 'Atas perhatian dan kerjasamanya, kami mengucapkan terima kasih.'.
  APPEND ls_text TO gt_text.
  ls_text-line = '</BODY></HTML>'.
  APPEND ls_text TO gt_text.

ENDFORM.

FORM f_create_body_email_english.

  DATA: ls_text TYPE LINE OF bcsy_text.

  ls_text-line = '<HTML><BODY>'.
  APPEND ls_text TO gt_text.
  ls_text-line = 'Dear Our Valuable Partner,'.
  APPEND ls_text TO gt_text.
  ls_text-line = '<br><br></br>'.
  APPEND ls_text TO gt_text.
  ls_text-line = 'Hereby we kindly send Purchase Order :'.
  APPEND ls_text TO gt_text.
  ls_text-line = '<br><br></br>'.
  APPEND ls_text TO gt_text.
  ls_text-line = '•   Please ensure the shipment schedules are followed completedly.'.
  APPEND ls_text TO gt_text.
  ls_text-line = '<br></br>'.
  APPEND ls_text TO gt_text.
  ls_text-line = '• If there is any potential delay or if you have any questions, please contact the person in charge at PT Suryaraya Rubberindo Industries.'.
  APPEND ls_text TO gt_text.
  ls_text-line = '<br><br></br>'.
  APPEND ls_text TO gt_text.

  ls_text-line = 'For Supplier import with Seafreight :'.
  APPEND ls_text TO gt_text.
  ls_text-line = '<br></br>'.
  APPEND ls_text TO gt_text.

  ls_text-line = '• Please ensure the Packing List, Invoice and Bill of Lading sent to us within max. 5 days since the vessel departure date.'.
  APPEND ls_text TO gt_text.
  ls_text-line = '<br></br>'.
  APPEND ls_text TO gt_text.
  ls_text-line = '• For Raw Materials Supplier, please use Surrender Bill of Lading. '.
  APPEND ls_text TO gt_text.
  ls_text-line = '<br><br></br>'.
  APPEND ls_text TO gt_text.

  ls_text-line = 'For Supplier import with Airfreight :'.
  APPEND ls_text TO gt_text.
  ls_text-line = '<br></br>'.
  APPEND ls_text TO gt_text.
  ls_text-line = '• Please ensure the Packing List, Invoice and Airway Bill sent to us within max. 3 days since the airplane departure date.'.
  APPEND ls_text TO gt_text.
  ls_text-line = '<br><br></br>'.
  APPEND ls_text TO gt_text.

  ls_text-line = 'Thankyou .'.
  APPEND ls_text TO gt_text.
  ls_text-line = '</BODY></HTML>'.
  APPEND ls_text TO gt_text.

ENDFORM.

FORM validation.

  SELECT * INTO TABLE @DATA(it_val) FROM zsrift_0001
    WHERE ebeln IN @p_ebeln.

  IF sy-subrc EQ 0.
    MESSAGE 'Document already Sent ! please check your document number' TYPE 'E'.
  ELSE.
    PERFORM send_mail.
  ENDIF.

ENDFORM.
