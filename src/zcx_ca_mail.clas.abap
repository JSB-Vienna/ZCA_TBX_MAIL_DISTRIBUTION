"! <p class="shorttext synchronized" lang="en">CA-TBX exception: Mail creation / distribution failed</p>
class ZCX_CA_MAIL definition
  public
  inheriting from ZCX_CA_PARAM
  create public .

public section.

  constants:
    begin of ZCX_CA_MAIL,
      msgid type symsgid value 'ZCA_TOOLBOX',
      msgno type symsgno value '037',
      attr1 type scx_attrname value '',
      attr2 type scx_attrname value '',
      attr3 type scx_attrname value '',
      attr4 type scx_attrname value '',
    end of ZCX_CA_MAIL .
  constants:
    begin of SET_REQ_ATTR_FAILED,
      msgid type symsgid value 'ZCA_TOOLBOX',
      msgno type symsgno value '038',
      attr1 type scx_attrname value '',
      attr2 type scx_attrname value '',
      attr3 type scx_attrname value '',
      attr4 type scx_attrname value '',
    end of SET_REQ_ATTR_FAILED .
  constants:
    begin of SET_MAIL_DOC_FAILED,
      msgid type symsgid value 'ZCA_TOOLBOX',
      msgno type symsgno value '039',
      attr1 type scx_attrname value '',
      attr2 type scx_attrname value '',
      attr3 type scx_attrname value '',
      attr4 type scx_attrname value '',
    end of SET_MAIL_DOC_FAILED .
  constants:
    begin of DISTRIBUTE_MAIL_FAILED,
      msgid type symsgid value 'ZCA_TOOLBOX',
      msgno type symsgno value '040',
      attr1 type scx_attrname value '',
      attr2 type scx_attrname value '',
      attr3 type scx_attrname value '',
      attr4 type scx_attrname value '',
    end of DISTRIBUTE_MAIL_FAILED .
  constants:
    begin of NO_RECIPIENTS,
      msgid type symsgid value 'ZCA_TOOLBOX',
      msgno type symsgno value '041',
      attr1 type scx_attrname value '',
      attr2 type scx_attrname value '',
      attr3 type scx_attrname value '',
      attr4 type scx_attrname value '',
    end of NO_RECIPIENTS .
  constants:
    begin of REC_OR_SENDER_WRONG,
      msgid type symsgid value 'ZCA_TOOLBOX',
      msgno type symsgno value '043',
      attr1 type scx_attrname value '',
      attr2 type scx_attrname value '',
      attr3 type scx_attrname value '',
      attr4 type scx_attrname value '',
    end of REC_OR_SENDER_WRONG .
  constants:
    begin of CREATION_SND_REQ_FAILED,
      msgid type symsgid value 'ZCA_TOOLBOX',
      msgno type symsgno value '042',
      attr1 type scx_attrname value '',
      attr2 type scx_attrname value '',
      attr3 type scx_attrname value '',
      attr4 type scx_attrname value '',
    end of CREATION_SND_REQ_FAILED .
  constants:
    begin of CREATION_MAIL_DOC_FAILED,
      msgid type symsgid value 'ZCA_TOOLBOX',
      msgno type symsgno value '044',
      attr1 type scx_attrname value '',
      attr2 type scx_attrname value '',
      attr3 type scx_attrname value '',
      attr4 type scx_attrname value '',
    end of CREATION_MAIL_DOC_FAILED .
  constants:
    begin of ADD_ATTACHM_FAILED,
      msgid type symsgid value 'ZCA_TOOLBOX',
      msgno type symsgno value '045',
      attr1 type scx_attrname value '',
      attr2 type scx_attrname value '',
      attr3 type scx_attrname value '',
      attr4 type scx_attrname value '',
    end of ADD_ATTACHM_FAILED .
  constants:
    begin of GROSS_SIZE_EXCEEDED,
      msgid type symsgid value 'ZCA_TOOLBOX',
      msgno type symsgno value '046',
      attr1 type scx_attrname value 'MV_MSGV1',
      attr2 type scx_attrname value '',
      attr3 type scx_attrname value '',
      attr4 type scx_attrname value '',
    end of GROSS_SIZE_EXCEEDED .
  constants:
    begin of ATTACHM_TOO_BIG,
      msgid type symsgid value 'ZCA_TOOLBOX',
      msgno type symsgno value '064',
      attr1 type scx_attrname value 'MV_MSGV1',
      attr2 type scx_attrname value 'MV_MSGV2',
      attr3 type scx_attrname value '',
      attr4 type scx_attrname value '',
    end of ATTACHM_TOO_BIG .
  constants:
    begin of RECIP_HAS_NO_MAIL_ADDR,
      msgid type symsgid value 'SWU_NOTIF',
      msgno type symsgno value '019',
      attr1 type scx_attrname value 'MV_MSGV1',
      attr2 type scx_attrname value '',
      attr3 type scx_attrname value '',
      attr4 type scx_attrname value '',
    end of RECIP_HAS_NO_MAIL_ADDR .
  "! <p class="shorttext synchronized" lang="en">My own name</p>
  constants C_ZCX_CA_MAIL type SEOCLSNAME value 'ZCX_CA_MAIL' ##NO_TEXT.

  "! <p class="shorttext synchronized" lang="en">CONSTRUCTOR</p>
  methods CONSTRUCTOR
    importing
      !TEXTID like IF_T100_MESSAGE=>T100KEY optional
      !PREVIOUS like PREVIOUS optional
      !MT_RETURN type BAPIRET2_T optional
      !MV_SUBRC type SYST_SUBRC optional
      !MV_MSGTY type SYMSGTY optional
      !MV_MSGV1 type SYMSGV optional
      !MV_MSGV2 type SYMSGV optional
      !MV_MSGV3 type SYMSGV optional
      !MV_MSGV4 type SYMSGV optional
      !MV_SEVERITY type T_SEVERITY optional .
protected section.
private section.
ENDCLASS.



CLASS ZCX_CA_MAIL IMPLEMENTATION.


  method CONSTRUCTOR.
CALL METHOD SUPER->CONSTRUCTOR
EXPORTING
PREVIOUS = PREVIOUS
MT_RETURN = MT_RETURN
MV_SUBRC = MV_SUBRC
MV_MSGTY = MV_MSGTY
MV_MSGV1 = MV_MSGV1
MV_MSGV2 = MV_MSGV2
MV_MSGV3 = MV_MSGV3
MV_MSGV4 = MV_MSGV4
MV_SEVERITY = MV_SEVERITY
.
clear me->textid.
if textid is initial.
  IF_T100_MESSAGE~T100KEY = ZCX_CA_MAIL .
else.
  IF_T100_MESSAGE~T100KEY = TEXTID.
endif.
  endmethod.
ENDCLASS.
