//Z13032SD  JOB CLASS=A,MSGCLASS=H,MSGLEVEL=(1,1),NOTIFY=Z13032,        00010011
//REGION-1000M,MEMLIMIT=2G                                              00020013
//SET LINKDSN=Z13032.LOAD                                               00040013
//STEP1    EXEC PGM=STUDTL07                                            00050010
//STEPLIB    DD DSN=&LINKDSN,DISP=SHR                                   00060001
//DDSTUDTL   DD DSN=Z13032.STUDENT.DETAILS,UNIT=SYSDA,                  00070006
//              DISP=(OLD,KEEP,KEEP)                                    00080006
//SYSOUT     DD SYSOUT=Y                                                00090014