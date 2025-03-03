*&---------------------------------------------------------------------*
*& Include          YSRILOP_R00033_TOP
*&---------------------------------------------------------------------*

TABLES: ekko, zsrift_0001.
INCLUDE ystdxdoi.
INCLUDE <icon>.

TYPES: BEGIN OF ty_headers,
         ebeln  TYPE ekko-ebeln,
         lifnr  TYPE ekko-lifnr,
         name1  TYPE lfa1-name1,
         revno  TYPE ekko-revno,
         aedat  TYPE ekko-aedat,
         ekgrp  TYPE ekko-ekgrp,
         waers  TYPE ekko-waers,
         frgke  TYPE c length 20,
         status TYPE c LENGTH 25,
         email type c length 30,
         icon   TYPE icon_d,
       END OF ty_headers.

TYPES: BEGIN OF ty_header,
         lifnr      TYPE ekko-lifnr,
         name1      TYPE lfa1-name1,
         name2      TYPE lfa1-name2,
         str_suppl1 TYPE adrc-str_suppl1,
         str_suppl2 TYPE adrc-str_suppl2,
         mc_city1   TYPE adrc-mc_city1,
         region     TYPE adrc-region,
         bezei      TYPE t005u-bezei,
         alamat     TYPE lfa1-stras,
         ebeln      TYPE ekko-ebeln,
         revno      TYPE ekko-revno,
         aedat      TYPE ekko-aedat,
         podesc     TYPE c LENGTH 100,
         verkf      TYPE lfm1-verkf,
         telfx      TYPE lfa1-telfx,
         smtp_addr  TYPE adr6-smtp_addr,
         subtotal   TYPE ekpo-netwr,
         zterm      TYPE ekko-zterm,
         ppn        TYPE i,
         waers      TYPE ekko-waers,
         total      TYPE ekpo-netwr,
         total_ppn  TYPE p LENGTH 15 DECIMALS 3,
         knumv      TYPE ekko-knumv,
         adrnr      TYPE lfa1-adrnr,
         keterangan TYPE char100,
         remarks    TYPE char100,
         frgke      TYPE ekko-frgke,
       END OF ty_header.

TYPES: BEGIN OF ty_newheader,
         ebeln    TYPE ekko-ebeln,
         revno    TYPE ekko-revno,
         aedat    TYPE ekko-aedat,
         zterm    TYPE ekko-zterm,
         subtotal TYPE ekpo-netwr,
       END OF ty_newheader.

TYPES: BEGIN OF ty_newdetail,
         ebelp     TYPE ekpo-ebelp,
         item      TYPE c LENGTH 500,
         menge     TYPE c LENGTH 10,
         meins     TYPE ekpo-meins,
         unitprice TYPE c LENGTH 20,
         amount    TYPE c LENGTH 20,
         sloctax   TYPE c LENGTH 15,
         ebeln     TYPE ekpo-ebeln,
         ppn       TYPE ekpo-netwr,
       END OF ty_newdetail.

TYPES : BEGIN OF ty_mwskz,
          mwskz TYPE ekpo-mwskz,
        END OF ty_mwskz.

TYPES: BEGIN OF cndp_user_info,
         user          TYPE ole2_parameter,           " Username am Server
         password      TYPE ole2_parameter,       " Password am Server
         proxy         TYPE ole2_parameter,          " Proxy (incl. Port)
         proxyuser     TYPE ole2_parameter,      "User am Proxy
         proxypassword TYPE ole2_parameter,  "Password am Proxy
         scrambled,                           " Flag ob verschlüsselt
       END OF cndp_user_info.

TYPES: BEGIN OF ty_data,
         no    TYPE zno,
         name1 TYPE lfa1-name1,
         ebeln TYPE ekko-ebeln,
*         aedat TYPE ekko-aedat,
         matnr TYPE ekpo-matnr,
         txz01 TYPE ekpo-txz01,
         meins TYPE ekpo-meins,
         eindt TYPE zddate,
         menge TYPE ekpo-menge,
         frgke TYPE c LENGTH 20,
*         total TYPE i,
       END OF ty_data.

DATA: it_data TYPE STANDARD TABLE OF ty_headers,
      wa_data type ty_headers,
      wa_pdf  TYPE ty_headers.

DATA: o_alv       TYPE REF TO cl_salv_table,
      o_selection TYPE REF TO cl_salv_selections.

DATA: o_oi        TYPE REF TO cl_office_integration,
      control     TYPE REF TO i_oi_container_control,
      proxy       TYPE REF TO i_oi_document_proxy,
      t_result    TYPE STANDARD TABLE OF zsriltt_00008,
      t_header    TYPE ty_header,
      lt_mwskz    TYPE c LENGTH 100,
      lv_top      TYPE c LENGTH 7,
      gv_idx      TYPE i,
      lv_ppn      TYPE c LENGTH 20,
      lv_total    TYPE c LENGTH 20,
      lv_subtotal TYPE c LENGTH 20,
      lt_mwskz2   TYPE STANDARD TABLE OF ty_mwskz.

DATA:
  lt_header TYPE zsriltt_00010,
  lp_header TYPE zsriltt_00011,
  lt_item   TYPE zsriltt_00009.

DATA:
  gt_header TYPE zsriltt_00011,
  gt_item   TYPE zsriltt_00009.

DATA: gv_formname TYPE tdsfname VALUE 'ZSRILSF_000003',
      gv_fm_name  TYPE rs38l_fnam.
DATA: gwa_ssfcompop TYPE ssfcompop,
      gwa_control   TYPE ssfctrlop.
DATA: gv_devtype TYPE rspoptype.
DATA: gv_job_output TYPE ssfcrescl.
DATA: gt_lines TYPE TABLE OF tline.
DATA: gv_size TYPE i.

* Define for fieldcatalog
DATA: fieldcatalog TYPE slis_t_fieldcat_alv WITH HEADER LINE.

DATA: wa_datas TYPE ty_data,
      lt_datas TYPE TABLE OF ty_data,
      new_line TYPE ty_data,
      filename TYPE string.

DATA: o_send_request TYPE REF TO cl_bcs,
      o_document     TYPE REF TO cl_document_bcs,
      o_sender       TYPE REF TO if_sender_bcs,
      o_recipient    TYPE REF TO if_recipient_bcs,
      cc_recipient   TYPE REF TO if_recipient_bcs,
      lr_sender      TYPE REF TO cl_cam_address_bcs.

DATA: gt_text TYPE bcsy_text,
      ls_text LIKE LINE OF gt_text.

SELECTION-SCREEN: BEGIN OF BLOCK aa WITH FRAME TITLE TEXT-001.
  SELECT-OPTIONS: p_ebeln FOR ekko-ebeln.
  SELECT-OPTIONS:p_ekgrp FOR ekko-ekgrp NO INTERVALS.
  SELECT-OPTIONS: p_aedat FOR ekko-aedat.
  PARAMETERS: p_report AS CHECKBOX.
SELECTION-SCREEN: END OF BLOCK aa.
