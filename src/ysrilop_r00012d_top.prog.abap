*&---------------------------------------------------------------------*
*& Include          ZSRIMM_R0001_TOP
*&---------------------------------------------------------------------*
TABLES: eban, prps, ebkn.

TYPES : BEGIN OF ty_data,
          ernam              TYPE eban-ernam, "Created By
          uname              TYPE usr03-name1, "Requisitioner
          loekz              TYPE eban-loekz, "Deletion Indicator
          ekgrp              TYPE eban-ekgrp, "Purchasing Group
          purreqndescription TYPE eban-purreqndescription, "PR Descr. header
          banfn              TYPE eban-banfn, "Purchase Requisition
          bnfpo              TYPE eban-bnfpo, "Requisition Date
          badat              TYPE eban-badat, "Line PR
          matnr              TYPE eban-matnr, "Material
          matkl              TYPE eban-matkl, "Material Group
          txz01              TYPE eban-txz01, "Short Text
          menge              TYPE eban-menge, "QTY PR
          bsmng              TYPE eban-bsmng,
          openqty            TYPE p DECIMALS 3,
          meins              TYPE eban-meins, "OuM
          preis              TYPE eban-preis, "Valuation Price
          totalamnt          TYPE eban-preis, "Total Value
          lfdat              TYPE eban-lfdat, "Delivery Date PR
          ps_psp_pnr         TYPE ebkn-ps_psp_pnr, "No WBS
          kostl              TYPE ebkn-kostl, "Cost center
          "----------------- Penambahan---------------
          appr1              TYPE c LENGTH 20, "Progress Approval
          appr2              TYPE c LENGTH 20, "Target Approval
          appr3              TYPE c LENGTH 20, "Status Release PR
          kasie              TYPE ekbe-budat, "Kasie PR
          kadept             TYPE ekbe-budat, "Kadept PR
          kadiv              TYPE ekbe-budat, "Kadiv PR
          dir1               TYPE ekbe-budat, "DIR 1
          dir2               TYPE ekbe-budat, "DIR 2
          dir3               TYPE ekbe-budat, "DIR 3
          "-----------------End------------------------
          frgdt              TYPE eban-frgdt, "Release date PR"
          ebeln              TYPE eban-ebeln, "PO Number"
          ebelp              TYPE eban-ebelp, "PO Line"
          aedat              TYPE ekko-aedat, "PO creation date"
          eindt              TYPE eket-eindt, "PO delivery date"
          lifnr              TYPE ekko-lifnr, "Vendor ID"
          name1              TYPE lfa1-name1, "Vendor Name"
          qtypo              TYPE ekpo-menge, "PO qty"
          wemng              TYPE eket-wemng, "GR qty from delivery schedule tab in ME22N/23N
          netp               TYPE p DECIMALS 3, "Quantity PO
          netpr              TYPE ekpo-netpr, "PO unit price"
          waers              TYPE ekko-waers, "PO curr"
          wkurs              TYPE ekko-wkurs, "PO rate"
          mwskz              TYPE ekpo-mwskz, "PO Tax Code"
          taxdescr           TYPE t007s-text1, "Tax code description"
          netwr              TYPE ekpo-netwr, "PO total amount"
          poreldate          TYPE ekko-aedat, "PO Release Date"
          postat             TYPE c LENGTH 20,
          belnr              TYPE ekbe-belnr, "GR Num"
          buzei              TYPE ekbe-buzei, "GR line"
          budat              TYPE ekbe-budat, "GR Date
          bwart              TYPE ekbe-bwart, "Mvt
*          qtygr      TYPE ekbe-menge, "GR Qty"
          qtygr              TYPE p DECIMALS 3, "GR Qty"
          dmbtr              TYPE ekbe-dmbtr, "GR amount local"
          wrbtr              TYPE ekbe-wrbtr, "GR amount ori"
        END OF ty_data.

DATA: it_data  TYPE STANDARD TABLE OF ty_data,
      it_data2 TYPE STANDARD TABLE OF ty_data,
      wa_data  TYPE ty_data,
      o_alv    TYPE REF TO cl_salv_table.

TYPES: BEGIN OF ty_out,
         number   TYPE banfn,
         relcode  TYPE frgco,
         descript TYPE frgct,
         icon     TYPE icon_d,
       END OF ty_out.

TYPES: BEGIN OF ty_ret,
         ernam              TYPE eban-ernam,
         loekz              TYPE eban-loekz,
         ekgrp              TYPE eban-ekgrp,
         purreqndescription TYPE eban-purreqndescription,
         banfn              TYPE eban-banfn,
         bnfpo              TYPE eban-bnfpo,
         badat              TYPE eban-badat,
         matnr              TYPE eban-matnr,
         matkl              TYPE eban-matkl,
         txz01              TYPE eban-txz01,
         bsmng              TYPE eban-bsmng,
         meins              TYPE eban-meins,
         preis              TYPE eban-preis,
         lfdat              TYPE eban-lfdat,
         ps_psp_pnr         TYPE ebkn-ps_psp_pnr,
         kostl              TYPE ebkn-kostl,
         ebeln              TYPE eban-ebeln,
         ebelp              TYPE eban-ebelp,
         frgkz              TYPE eban-frgkz,
         frgzu              TYPE eban-frgzu,
         aedat              TYPE ekko-aedat,
         eindt              TYPE eket-eindt,
         wemng              TYPE eket-wemng,
         lifnr              TYPE ekko-lifnr,
         name1              TYPE lfa1-name1,
         netpr              TYPE ekpo-netpr,
         waers              TYPE ekko-waers,
         wkurs              TYPE ekko-wkurs,
         mwskz              TYPE ekpo-mwskz,
         netwr              TYPE ekpo-netwr,
         belnr              TYPE ekbe-belnr,
         buzei              TYPE ekbe-buzei,
         budat              TYPE ekbe-budat,
         bwart              TYPE ekbe-bwart,
*         menge      TYPE ekbe-menge,
         menge              TYPE p DECIMALS 3,
         dmbtr              TYPE ekbe-dmbtr,
         wrbtr              TYPE ekbe-wrbtr,
         gjahr              TYPE ekbe-gjahr,
       END OF ty_ret.

DATA: it_ret  TYPE TABLE OF ty_ret,
      it_ret2 TYPE TABLE OF ty_ret,
      wa_ret  TYPE ty_ret.

DATA: gr_table    TYPE REF TO cl_salv_table.

DATA: lr_columns TYPE REF TO cl_salv_columns_table,
      lr_column  TYPE REF TO cl_salv_column_table.

DATA: lt_final  TYPE TABLE OF bapirlcorq,
      lt_out    TYPE TABLE OF ty_out,
      ls_out    TYPE ty_out,
      ls_posted LIKE TABLE OF bapirlcorq WITH HEADER LINE,
      wa_posted TYPE bapirlcorq,
      ls_final  TYPE bapirlcorq.

SELECTION-SCREEN BEGIN OF BLOCK aa WITH FRAME TITLE TEXT-001.
  SELECT-OPTIONS: s_prnum FOR eban-ernam.
  SELECT-OPTIONS: s_badat FOR eban-badat.
  SELECT-OPTIONS: s_matnr FOR eban-matnr.
  SELECT-OPTIONS : s_wbs  FOR ebkn-ps_psp_pnr.
  PARAMETERS: listbox(10) AS LISTBOX VISIBLE LENGTH 15 .
SELECTION-SCREEN END OF BLOCK aa.

AT SELECTION-SCREEN OUTPUT.
  DATA: name  TYPE vrm_id,
        list  TYPE vrm_values,
        value TYPE vrm_value.

  name = 'LISTBOX'.

  value-key = '2'.
  value-text = 'Released'.
  APPEND value TO list.
  value-key = 'X'.
  value-text = 'Incomplete'.
  APPEND value TO list.
  value-key = ''.
  value-text = 'Draft'.
  APPEND value TO list.
  value-key = '1'.
  value-text = 'All'.
  APPEND value TO list.

  CALL FUNCTION 'VRM_SET_VALUES'
    EXPORTING
      id              = name
      values          = list
    EXCEPTIONS
      id_illegal_name = 0
      OTHERS          = 0.

  listbox = '1'.
