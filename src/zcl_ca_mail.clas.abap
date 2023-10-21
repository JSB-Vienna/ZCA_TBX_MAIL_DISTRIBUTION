"! <p class="shorttext synchronized" lang="en">CA-TBX: Mail distribution</p>
CLASS zcl_ca_mail DEFINITION PUBLIC
                             CREATE PUBLIC.

* P U B L I C   S E C T I O N
  PUBLIC SECTION.
*   i n t e r f a c e s
    INTERFACES:
      zif_ca_c_doc_class.

*   a l i a s e s
    ALIASES:
*     Document classes, font, font types and sizes
      c_docclass_htm          FOR  zif_ca_c_doc_class~c_docclass_htm,
      c_docclass_raw          FOR  zif_ca_c_doc_class~c_docclass_raw.

*   c o n s t a n t s
    CONSTANTS:
      "! <p class="shorttext synchronized" lang="en">Requested status: Never</p>
      c_reqstat_never   TYPE bcs_rqst          VALUE 'N'  ##no_text,
      "! <p class="shorttext synchronized" lang="en">Requested status: Only if errors occur</p>
      c_reqstat_error   TYPE bcs_rqst          VALUE 'E'  ##no_text,
      "! <p class="shorttext synchronized" lang="en">Requested status: If sent</p>
      c_reqstat_sent    TYPE bcs_rqst          VALUE 'D'  ##no_text,
      "! <p class="shorttext synchronized" lang="en">Requested status: If read</p>
      c_reqstat_read    TYPE bcs_rqst          VALUE 'R'  ##no_text,
      "! <p class="shorttext synchronized" lang="en">Requested status: Always</p>
      c_reqstat_always  TYPE bcs_rqst          VALUE 'A'  ##no_text,
      "! <p class="shorttext synchronized" lang="en">Status mail: Never</p>
      c_statmail_never  TYPE bcs_stml          VALUE 'N'  ##no_text,
      "! <p class="shorttext synchronized" lang="en">Status mail: Only if errors occur</p>
      c_statmail_error  TYPE bcs_stml          VALUE 'E'  ##no_text,
      "! <p class="shorttext synchronized" lang="en">Status mail: If sent</p>
      c_statmail_sent   TYPE bcs_stml          VALUE 'D'  ##no_text,
      "! <p class="shorttext synchronized" lang="en">Status mail: If read</p>
      c_statmail_read   TYPE bcs_stml          VALUE 'R'  ##no_text,
      "! <p class="shorttext synchronized" lang="en">Status mail: Always</p>
      c_statmail_always TYPE bcs_stml          VALUE 'A'  ##no_text.

*   i n s t a n c e   a t t r i b u t e s
    DATA:
*     o b j e c t   r e f e r e n c e s
      "! <p class="shorttext synchronized" lang="en">Constants for commit mode</p>
      mo_commit_modes   TYPE REF TO zcl_ca_c_commit_mode.

*   i n s t a n c e   m e t h o d s
    METHODS:
      "! <p class="shorttext synchronized" lang="en">Add recipient</p>
      "!
      "! @parameter is_recipient | <p class="shorttext synchronized" lang="en">Mail recipient</p>
      "! @raising   zcx_ca_mail  | <p class="shorttext synchronized" lang="en">Common exception: Mail creation / distribution failed</p>
      add_recipient
        IMPORTING
          is_recipient TYPE zca_s_mail_recipient
        RAISING
          zcx_ca_mail,

      "! <p class="shorttext synchronized" lang="en">Attach single document, e. g. direct upload</p>
      "!
      "! @parameter iv_attachm_type | <p class="shorttext synchronized" lang="en">SAP office document class (see table TSOTD)</p>
      "! @parameter iv_attachm_name | <p class="shorttext synchronized" lang="en">Subject / file name of attachment (limited to 50 digits!!)</p>
      "! @parameter iv_attachm_size | <p class="shorttext synchronized" lang="en">Size of attachment</p>
      "! @parameter it_attachm_text | <p class="shorttext synchronized" lang="en">Attachment content in character format</p>
      "! @parameter it_attachm_hex  | <p class="shorttext synchronized" lang="en">Attachment content in hex/binary format</p>
      "! @raising   zcx_ca_mail     | <p class="shorttext synchronized" lang="en">Common exception: Mail creation / distribution failed</p>
      attach_doc
        IMPORTING
          iv_attachm_type TYPE so_obj_tp DEFAULT 'EXT' ##no_text
          iv_attachm_name TYPE so_obj_des
          iv_attachm_size TYPE i OPTIONAL
          it_attachm_text TYPE soli_tab OPTIONAL
          it_attachm_hex  TYPE solix_tab OPTIONAL
        RAISING
          zcx_ca_mail,

      "! <p class="shorttext synchronized" lang="en">Attach archived documents</p>
      "!
      "! @parameter it_archive_docs        | <p class="shorttext synchronized" lang="en">ArchiveLink and DMS document instances</p>
      "! @parameter iv_max_file_size       | <p class="shorttext synchronized" lang="en">Max. size of a single attachment (2 MB, 0 = every size!)</p>
      "! @parameter iv_max_mail_size       | <p class="shorttext synchronized" lang="en">Max. size of the complete email (12,5 MB)</p>
      "! @parameter iv_raise_exc_mail_size | <p class="shorttext synchronized" lang="en">X = Raise exception, when mail size is exceeded</p>
      "! @parameter result                 | <p class="shorttext synchronized" lang="en">Number of skipped attachments - too large, not found, etc.</p>
      "! @raising   zcx_ca_mail            | <p class="shorttext synchronized" lang="en">Common exception: Mail creation / distribution failed</p>
      "! @raising   zcx_ca_archive_content | <p class="shorttext synchronized" lang="en">Common exception: Error while handling ArchiveLink content</p>
      attach_docs_from_archive
        IMPORTING
          it_archive_docs        TYPE zca_tt_archive_docs
          iv_max_file_size       TYPE zca_d_max_file_size DEFAULT 2097152
          iv_max_mail_size       TYPE zca_d_max_mail_size DEFAULT 13107200
          iv_raise_exc_mail_size TYPE abap_bool           DEFAULT abap_true
        RETURNING
          VALUE(result)          TYPE i
        RAISING
          zcx_ca_mail
          zcx_ca_archive_content,

      "! <p class="shorttext synchronized" lang="en">Constructor</p>
      "!
      "! @parameter iv_sender     | <p class="shorttext synchronized" lang="en">User id of sender (use e. g. in BG to repl WF-BATCH/SAP_WFRT</p>
      "! @parameter iv_reply_to   | <p class="shorttext synchronized" lang="en">Reply address if different from sender address</p>
      "! @parameter it_recipients | <p class="shorttext synchronized" lang="en">Mail recepients</p>
      "! @raising   zcx_ca_mail   | <p class="shorttext synchronized" lang="en">Common exception: Mail creation / distribution failed</p>
      constructor
        IMPORTING
          iv_sender     TYPE uname DEFAULT sy-uname
          iv_reply_to   TYPE ad_smtpadr OPTIONAL
          it_recipients TYPE zca_tt_mail_recipients OPTIONAL
        RAISING
          zcx_ca_mail,

      "! <p class="shorttext synchronized" lang="en">Get mail document</p>
      "!
      "! @parameter result | <p class="shorttext synchronized" lang="en">Mail</p>
      get_mail
        RETURNING
          VALUE(result) TYPE REF TO cl_document_bcs,

      "! <p class="shorttext synchronized" lang="en">Get send request</p>
      "!
      "! @parameter result | <p class="shorttext synchronized" lang="en">Send request</p>
      get_request
        RETURNING
          VALUE(result) TYPE REF TO cl_bcs,

      "! <p class="shorttext synchronized" lang="en">Send mail</p>
      "!
      "! @parameter iv_immediately | <p class="shorttext synchronized" lang="en">X = Send mail immediately</p>
      "! @parameter iv_req_status  | <p class="shorttext synchronized" lang="en">Requested Status (use const C_REQSTAT_*)</p>
      "! @parameter iv_status_mail | <p class="shorttext synchronized" lang="en">Setting which status are reported by mail (use C_STATMAIL_*)</p>
      "! @parameter iv_commit_mode | <p class="shorttext synchronized" lang="en">Commi mode (use const. C_COMMIT_MODE_*)</p>
      "! @raising   zcx_ca_mail    | <p class="shorttext synchronized" lang="en">Common exception: Mail creation / distribution failed</p>
      send
        IMPORTING
          iv_immediately TYPE abap_bool DEFAULT abap_false
          iv_req_status  TYPE bcs_rqst  DEFAULT c_reqstat_never
          iv_status_mail TYPE bcs_stml  DEFAULT c_statmail_never
          iv_commit_mode TYPE zca_d_commit_mode DEFAULT zcl_ca_c_commit_mode=>asynchron
        RAISING
          zcx_ca_mail,

      "! <p class="shorttext synchronized" lang="en">Set mail document</p>
      "!
      "! @parameter io_mail | <p class="shorttext synchronized" lang="en">Mail</p>
      set_mail
        IMPORTING
          io_mail TYPE REF TO cl_document_bcs,

      "! <p class="shorttext synchronized" lang="en">Set send request</p>
      "!
      "! @parameter io_snd_req | <p class="shorttext synchronized" lang="en">Send request</p>
      set_request
        IMPORTING
          io_snd_req TYPE REF TO cl_bcs,

      "! <p class="shorttext synchronized" lang="en">Set subject and mail body</p>
      "!
      "! @parameter iv_subject       | <p class="shorttext synchronized" lang="en">Subject (restricted to 50 char, if distr. via SAP connect)</p>
      "! @parameter it_mail_text     | <p class="shorttext synchronized" lang="en">Mail text</p>
      "! @parameter it_mail_text_hex | <p class="shorttext synchronized" lang="en">Mail text in hex / binary</p>
      "! @parameter iv_priority      | <p class="shorttext synchronized" lang="en">Document priority -> 1 = high, 5 = medium, 9 = low</p>
      "! @parameter iv_doc_class     | <p class="shorttext synchronized" lang="en">Document class (use const. C_DOCCLASS_*)</p>
      "! @raising   zcx_ca_mail      | <p class="shorttext synchronized" lang="en">Common exception: Mail creation / distribution failed</p>
      set_text
        IMPORTING
          iv_subject       TYPE string
          it_mail_text     TYPE soli_tab OPTIONAL
          it_mail_text_hex TYPE solix_tab OPTIONAL
          iv_priority      TYPE bcs_docimp DEFAULT '5'
          iv_doc_class     TYPE so_obj_tp DEFAULT zcl_ca_mail=>c_docclass_htm
        RAISING
          zcx_ca_mail.


* P R O T E C T E D   S E C T I O N
  PROTECTED SECTION.
*   c o n s t a n t s
    CONSTANTS:
      "! <p class="shorttext synchronized" lang="en">Application Log: Subobject to ZCA: Mailing</p>
      c_subobj_mailing  TYPE balsubobj         VALUE 'MAILING' ##no_text.

*   i n s t a n c e   a t t r i b u t e s
    DATA:
*     o b j e c t   r e f e r e n c e s
      "! <p class="shorttext synchronized" lang="en">Agent Send Request</p>
      mo_bcs_queue_agent TYPE REF TO ca_queue_entry_bcs,
      "! <p class="shorttext synchronized" lang="en">Common object: Application log (BAL)</p>
      mo_log             TYPE REF TO zcl_ca_log,
      "! <p class="shorttext synchronized" lang="en">Wrapper Class for Office Documents</p>
      mo_mail            TYPE REF TO cl_document_bcs,
      "! <p class="shorttext synchronized" lang="en">Business Communication Service</p>
      mo_snd_req         TYPE REF TO cl_bcs,

*     s i n g l e   v a l u e s
      "! <p class="shorttext synchronized" lang="en">Name of Character Set</p>
      mv_charset         TYPE cpcsname,
      "! <p class="shorttext synchronized" lang="en">Mail size</p>
      mv_mail_size       TYPE i,
      "! <p class="shorttext synchronized" lang="en">Long subject (at the end limited to 255 digits)</p>
      mv_subject_long    TYPE string.

*   i n s t a n c e   m e t h o d s
    METHODS:
      "! <p class="shorttext synchronized" lang="en">Add file extension and shorten the name to 50 digits</p>
      "!
      "! @parameter iv_file_ext     | <p class="shorttext synchronized" lang="en">Sequential number of current document</p>
      "! @parameter iv_attachm_name | <p class="shorttext synchronized" lang="en">Business object type and key</p>
      "! @parameter result          | <p class="shorttext synchronized" lang="en">Prepared attachment name</p>
      finalize_attachm_name
        IMPORTING
          iv_file_ext     TYPE saedoktyp
          iv_attachm_name TYPE bcsd_subj
        RETURNING
          VALUE(result)   TYPE bcsd_subj,

      "! <p class="shorttext synchronized" lang="en">Assemble attachment name without extension</p>
      "!
      "! @parameter iv_doc_cnt | <p class="shorttext synchronized" lang="en">Sequential number of current document</p>
      "! @parameter io_doc     | <p class="shorttext synchronized" lang="en">ArchiveLink + DMS: Archived document</p>
      "! @parameter result     | <p class="shorttext synchronized" lang="en">Assembled attachment name without file extension</p>
      assemble_attachm_name
        IMPORTING
          iv_doc_cnt    TYPE numc2
          io_doc        TYPE REF TO zif_ca_archive_doc
        RETURNING
          VALUE(result) TYPE bcsd_subj.

ENDCLASS.



CLASS zcl_ca_mail IMPLEMENTATION.


  METHOD add_recipient.
    "-----------------------------------------------------------------*
    "   Add recipient
    "-----------------------------------------------------------------*
    TRY.
        "Create recipient from mail address and ...
        DATA(lo_recip) =
             cl_cam_address_bcs=>create_internet_address(
                                      i_address_string = is_recipient-mail_addr ).

        "... add to send request
        mo_snd_req->add_recipient( i_recipient  = lo_recip
                                   i_express    = is_recipient-express
                                   i_copy       = is_recipient-as_copy
                                   i_blind_copy = is_recipient-as_blind_copy
                                   i_no_forward = is_recipient-no_forward ).

        "Increase round mail size
        mv_mail_size = mv_mail_size + strlen( lo_recip->if_sender_bcs~address_string( ) ).

      CATCH cx_address_bcs
            cx_send_req_bcs INTO DATA(lx_error).
        "Setting recipient or sender failed - check inbound data
        RAISE EXCEPTION TYPE zcx_ca_mail
          EXPORTING
            textid   = zcx_ca_mail=>rec_or_sender_wrong
            previous = lx_error.
    ENDTRY.
  ENDMETHOD.                    "add_recipient


  METHOD assemble_attachm_name.
    "-----------------------------------------------------------------*
    "   Assemble attachment name without extension (individualize
    "   by inheriting this class and redefine this method)
    "-----------------------------------------------------------------*
    result = |{ io_doc->mbo_document-instid }_{ iv_doc_cnt }|.
  ENDMETHOD.                    "assemble_attachm_name


  METHOD attach_doc.
    "-----------------------------------------------------------------*
    "   Attach single document, e. g. direct upload
    "-----------------------------------------------------------------*
    "Local data definitions
    DATA:
      lv_attachm_size_n TYPE n LENGTH 12,
      lv_attachm_size   TYPE so_obj_len.

    TRY.
        "Convert size into a character field
        lv_attachm_size_n = iv_attachm_size.
        lv_attachm_size   = lv_attachm_size_n.

        "Attach document to mail
        mo_mail->add_attachment( i_attachment_type    = iv_attachm_type
                                 i_attachment_size    = lv_attachm_size
                                 i_attachment_subject = iv_attachm_name
                                 i_att_content_text   = it_attachm_text
                                 i_att_content_hex    = it_attachm_hex ).

      CATCH cx_document_bcs INTO DATA(lx_error).
        "Adding an attachment failed
        RAISE EXCEPTION TYPE zcx_ca_mail
          EXPORTING
            textid   = zcx_ca_mail=>add_attachm_failed
            previous = lx_error.
    ENDTRY.
  ENDMETHOD.                    "attach_doc


  METHOD attach_docs_from_archive.
    "-----------------------------------------------------------------*
    "   Attach archived documents
    "-----------------------------------------------------------------*
    "Local data definitions
    DATA:
      lx_mail_err      TYPE REF TO zcx_ca_mail,
      lv_file_ext      TYPE saedoktyp,
      lv_doc_cnt       TYPE numc2,
      lv_max_mail_size TYPE p LENGTH 8 DECIMALS 3.

    "Get each document from archive, convert it into hex format and attach it
    CLEAR result.
    LOOP AT it_archive_docs INTO DATA(lo_doc).
      TRY.
          DATA(lv_doc_bin) = lo_doc->get_document( ).

          "Skip documents of a larger size than the requested size
          IF iv_max_file_size      NE 0                 AND
             lo_doc->mv_doc_length GT iv_max_file_size.
            "Attachment has a file size of &1MB, but only such up to &2MB are allowed
            RAISE EXCEPTION TYPE zcx_ca_mail
              EXPORTING
                textid   = zcx_ca_mail=>attachm_too_big
                mv_msgv1 = CONV #( |{ lo_doc->mv_doc_length / 1024 ZERO = NO }| )
                mv_msgv2 = CONV #( |{ iv_max_file_size / 1024 ZERO = NO }| ).
          ENDIF.

          "Reconvert this hex string into a binary table for distribution via SAP connect
          DATA(lt_attachm_doc) = cl_document_bcs=>xstring_to_solix( lv_doc_bin ).

          "Set file extension
          IF lo_doc->ms_data-reserve EQ 'FAX' ##no_text.
            lv_file_ext = 'TIF' ##no_text.
          ELSE.
            lv_file_ext = lo_doc->ms_data-reserve.
          ENDIF.

          "Create file name, add file extension and shorten to 50 digits
          ADD 1 TO lv_doc_cnt.
          DATA(lv_attachm_name) = finalize_attachm_name( iv_file_ext     = lv_file_ext
                                                         iv_attachm_name = assemble_attachm_name(
                                                                                       iv_doc_cnt = lv_doc_cnt
                                                                                       io_doc     = lo_doc ) ).

          mv_mail_size = mv_mail_size + lo_doc->mv_doc_length.
          "Check if overall mail size is exceeded
          IF mv_mail_size GT iv_max_mail_size.
            CASE iv_raise_exc_mail_size.
              WHEN abap_false.
                ADD 1 TO result.

              WHEN abap_true.
                lv_max_mail_size = iv_max_mail_size / 1048576.  "Divide by 1 MB
                WRITE lv_max_mail_size ROUND 1 DECIMALS 1
                                       LEFT-JUSTIFIED TO sy-msgv1.
                "Max. size of &1 for a mail is exceeded
                RAISE EXCEPTION TYPE zcx_ca_mail
                  EXPORTING
                    textid   = zcx_ca_mail=>gross_size_exceeded
                    mv_msgv1 = sy-msgv1.
            ENDCASE.
          ENDIF.

          "Attach document to mail
          mo_mail->add_attachment(
                            i_attachment_type    = COND #(
                                                     WHEN lo_doc->ms_doc_type_def-doc_type IS NOT INITIAL
                                                       THEN lo_doc->ms_doc_type_def-doc_type
                                                       ELSE 'EXT' )   ##no_text
                            i_attachment_size    = CONV #( |{ CONV num12( lo_doc->mv_doc_length ) }| )
                            i_attachment_subject = CONV #( lv_attachm_name )
                            i_att_content_hex    = lt_attachm_doc ).


        CATCH zcx_ca_error INTO DATA(lx_error).
          mo_log->add_msg_exc( lx_error ).
          ADD 1 TO result.

        CATCH cx_document_bcs INTO DATA(lx_bcs_error).
          mo_log->add_msg_exc( lx_bcs_error ).
          ADD 1 TO result.
      ENDTRY.
    ENDLOOP.

    IF result GT 0.
      mo_log->save( iv_commit = abap_false ).
    ENDIF.
  ENDMETHOD.                    "attach_docs_from_archive


  METHOD constructor.
    "-----------------------------------------------------------------*
    "   Constructor
    "-----------------------------------------------------------------*
    "Local data definitions
    DATA:
      lx_error  TYPE REF TO cx_root,
      lo_sender TYPE REF TO cl_sapuser_bcs.

    "All mailing exceptions inherit from the super exception class CX_BCS,
    "so it is enough to catch this exception.
    TRY.
        "Create logging and send request
        mo_log = NEW zcl_ca_log( iv_object = zif_ca_c_log_techn=>c_object_zca
                                 iv_subobj = c_subobj_mailing ).

        mo_snd_req = cl_bcs=>create_persistent( ).

        LOOP AT it_recipients INTO DATA(ls_recipient).
          "Create recipients
          add_recipient( is_recipient = ls_recipient ).
        ENDLOOP.

        "Set reply address if requested
        IF iv_reply_to IS NOT INITIAL.
          DATA(lo_reply_to) = cl_cam_address_bcs=>create_internet_address( i_address_string = iv_reply_to ).
          mo_snd_req->set_reply_to( lo_reply_to ).

          "Increase round mail size
          mv_mail_size = mv_mail_size + strlen( lo_reply_to->if_sender_bcs~address_string( ) ).
        ENDIF.

        "Create and set sender
        lo_sender = cl_sapuser_bcs=>create( iv_sender ).
        mo_snd_req->set_sender( lo_sender ).

        "Increase round mail size (is only the users name, but better then nothing :()
        mv_mail_size = mv_mail_size + strlen( lo_sender->if_sender_bcs~address_name( ) ).

        mo_commit_modes = zcl_ca_c_commit_mode=>get_instance( ).

      CATCH cx_address_bcs INTO lx_error.
        "Setting recipient or sender failed - check inbound data
        RAISE EXCEPTION TYPE zcx_ca_mail
          EXPORTING
            textid   = zcx_ca_mail=>rec_or_sender_wrong
            previous = lx_error.

      CATCH cx_send_req_bcs INTO lx_error.
        "Creation of sending request failed
        RAISE EXCEPTION TYPE zcx_ca_mail
          EXPORTING
            textid   = zcx_ca_mail=>creation_snd_req_failed
            previous = lx_error.
    ENDTRY.
  ENDMETHOD.                    "constructor


  METHOD finalize_attachm_name.
    "-----------------------------------------------------------------*
    "   Add file extension and shorten the name to 50 digits
    "-----------------------------------------------------------------*
    "Local data definitions
    DATA:
      lv_file_ext          TYPE saedoktyp.

    CALL FUNCTION 'CV120_SPLIT_FILE'
      EXPORTING
        pf_file       = iv_attachm_name
      IMPORTING
        pfx_file      = result     "File name w/o extension
        pfx_extension = lv_file_ext.

    "Imported extension is always in upper case. If extensions are equal
    "use original name.
    lv_file_ext = |{ lv_file_ext CASE = UPPER }|.
    IF lv_file_ext EQ iv_file_ext.
      result = iv_attachm_name.
    ELSE.
      result = |{ result }.{ iv_file_ext }|.
      lv_file_ext = iv_file_ext.
    ENDIF.

    "Result is always a file name with extension. So no need to respect file
    "extension separately.
    DATA(lv_len_an) = strlen( result ).    "Length assembled attachment name

    IF lv_len_an GT 50.
      "Calc. half length of max. attachment name minus half of the placeholder
      DATA(lv_len_shh)  = ( 50 DIV 2 ) - 2.
      DATA(lv_off_rear) = lv_len_an - lv_len_shh + 1.

      result = |{ result(lv_len_shh) } ... { result+lv_off_rear(lv_len_shh) }|.
    ENDIF.
  ENDMETHOD.                    "finalize_attachm_name


  METHOD get_mail.
    "-----------------------------------------------------------------*
    "   Get mail document
    "-----------------------------------------------------------------*
    result = mo_mail.
  ENDMETHOD.                    "get_mail


  METHOD get_request.
    "-----------------------------------------------------------------*
    "   Get send request
    "-----------------------------------------------------------------*
    result = mo_snd_req.
  ENDMETHOD.                    "get_request


  METHOD send.
    "-----------------------------------------------------------------*
    "   Send mail
    "-----------------------------------------------------------------*
    "Local data definitions
    DATA:
      lx_error  TYPE REF TO cx_root.

    TRY.
        mo_commit_modes->is_valid( iv_commit_mode ).

        IF mo_snd_req->recipients( ) IS INITIAL.
          "No recipients denoted
          RAISE EXCEPTION TYPE zcx_ca_mail
            EXPORTING
              textid = zcx_ca_mail=>no_recipients.
        ENDIF.

        "Set completed mail (body, subject, attachments) into send request
        mo_snd_req->set_document( mo_mail ).

      CATCH cx_send_req_bcs INTO lx_error.
        "Set mail document into send request failed
        RAISE EXCEPTION TYPE zcx_ca_mail
          EXPORTING
            textid   = zcx_ca_mail=>set_mail_doc_failed
            previous = lx_error.
    ENDTRY.

    TRY.
        "Return status after distribution? NO!!
        mo_snd_req->set_status_attributes(
                            i_requested_status = COND #(
                                                   WHEN iv_req_status IS INITIAL
                                                     THEN c_reqstat_never
                                                     ELSE iv_req_status )
                            i_status_mail      = COND #(
                                                   WHEN iv_status_mail IS INITIAL
                                                     THEN c_statmail_never
                                                     ELSE iv_status_mail ) ) ##no_text.
        "Distribute immediately?
        mo_snd_req->set_send_immediately( iv_immediately ).

      CATCH cx_send_req_bcs INTO lx_error.
        "Set attributes to mail request failed
        RAISE EXCEPTION TYPE zcx_ca_mail
          EXPORTING
            textid   = zcx_ca_mail=>set_req_attr_failed
            previous = lx_error.
    ENDTRY.

    TRY.
        "Send mail via SAP connect in any case. The send request is then
        "visible in transaction SOST.
        mo_snd_req->send( ).

        "Commit work to complete send request
        zcl_ca_utils=>do_commit( iv_commit_mode = iv_commit_mode ).

      CATCH cx_send_req_bcs
            zcx_ca_param INTO lx_error.
        "Sending mail failed
        RAISE EXCEPTION TYPE zcx_ca_mail
          EXPORTING
            textid   = zcx_ca_mail=>distribute_mail_failed
            previous = lx_error.
    ENDTRY.
  ENDMETHOD.                    "send


  METHOD set_mail.
    "-----------------------------------------------------------------*
    "   Set mail document
    "-----------------------------------------------------------------*
    mo_mail = io_mail.
  ENDMETHOD.                    "set_mail


  METHOD set_request.
    "-----------------------------------------------------------------*
    "   Set send request
    "-----------------------------------------------------------------*
    IF mo_snd_req IS BOUND.
      mo_snd_req->delete( ).
    ENDIF.

    mo_snd_req = io_snd_req.
  ENDMETHOD.                    "set_request


  METHOD set_text.
    "-----------------------------------------------------------------*
    "   Set subject and mail body
    "-----------------------------------------------------------------*
    "Local data definitions
    DATA:
      lx_error    TYPE REF TO cx_root,
      ls_doc_attr TYPE bcss_dbpa,
      lv_subject  TYPE so_obj_des.

    "Check allowed document classes
    IF iv_doc_class NE c_docclass_raw AND
       iv_doc_class NE c_docclass_htm.
      "Parameter '&1' has invalid value '&2'
      RAISE EXCEPTION TYPE zcx_ca_mail
        EXPORTING
          textid   = zcx_ca_mail=>param_invalid
          mv_msgv1 = 'IV_DOC_CLASS' ##no_text
          mv_msgv2 = CONV #( iv_doc_class ).
    ENDIF.

    TRY.
        "Create mail document
        mv_subject_long = lv_subject = iv_subject.
        "If no subject is passed here, the mail will have no subject, although the
        "subject is passed here after as long subject.
        mo_mail = cl_document_bcs=>create_document( i_subject    = lv_subject
                                                    i_text       = it_mail_text
                                                    i_hex        = it_mail_text_hex
                                                    i_importance = iv_priority
                                                    i_type       = iv_doc_class ).

        "Set long subject, limited to 255 digits
        mo_snd_req->set_message_subject( mv_subject_long ).

        "Get attributes of mail body or attachment
        ls_doc_attr = mo_mail->if_document_bcs~get_body_part_attributes( 1 ).
        mv_mail_size = mv_mail_size + ls_doc_attr-docsize + strlen( mv_subject_long ).

      CATCH cx_document_bcs
            cx_send_req_bcs INTO lx_error.
        "Creating mail document failed
        RAISE EXCEPTION TYPE zcx_ca_mail
          EXPORTING
            textid   = zcx_ca_mail=>creation_mail_doc_failed
            previous = lx_error.
    ENDTRY.
  ENDMETHOD.                    "set_text

ENDCLASS.
