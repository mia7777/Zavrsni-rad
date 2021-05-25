  MEMBER('Krojacnica.clw')

  INCLUDE('ABFILE.INC'),ONCE

  MAP
Krojacnica_BC0:DctInit    PROCEDURE
Krojacnica_BC0:DctKill    PROCEDURE
Krojacnica_BC0:FilesInit  PROCEDURE
  END

Hide:Access:TKANINA  CLASS(FileManager),TYPE               ! FileManager for TKANINA
Init                   PROCEDURE(),DERIVED
Kill                   PROCEDURE(),DERIVED
UseFile                PROCEDURE(BYTE UseType = UseType:Uses),BYTE,PROC,DERIVED
                     END


Hide:Relate:TKANINA  CLASS(RelationManager),TYPE           ! RelationManager for TKANINA
Init                   PROCEDURE
DeferedAddRelations    PROCEDURE(),DERIVED
Kill                   PROCEDURE(),DERIVED
                     END

Hide:Access:SIVA     CLASS(FileManager),TYPE               ! FileManager for SIVA
Init                   PROCEDURE(),DERIVED
Kill                   PROCEDURE(),DERIVED
PrimeFields            PROCEDURE(),PROC,DERIVED
UseFile                PROCEDURE(BYTE UseType = UseType:Uses),BYTE,PROC,DERIVED
ValidateFieldServer    PROCEDURE(UNSIGNED Id,BYTE HandleErrors),BYTE,PROC,DERIVED
                     END


Hide:Relate:SIVA     CLASS(RelationManager),TYPE           ! RelationManager for SIVA
Init                   PROCEDURE
DeferedAddRelations    PROCEDURE(),DERIVED
Kill                   PROCEDURE(),DERIVED
                     END

Hide:Access:KLIJENT  CLASS(FileManager),TYPE               ! FileManager for KLIJENT
Init                   PROCEDURE(),DERIVED
Kill                   PROCEDURE(),DERIVED
UseFile                PROCEDURE(BYTE UseType = UseType:Uses),BYTE,PROC,DERIVED
                     END


Hide:Relate:KLIJENT  CLASS(RelationManager),TYPE           ! RelationManager for KLIJENT
Init                   PROCEDURE
DeferedAddRelations    PROCEDURE(),DERIVED
Kill                   PROCEDURE(),DERIVED
                     END

Hide:Access:POPRAVLJA CLASS(FileManager),TYPE              ! FileManager for POPRAVLJA
Init                   PROCEDURE(),DERIVED
Kill                   PROCEDURE(),DERIVED
PrimeFields            PROCEDURE(),PROC,DERIVED
UseFile                PROCEDURE(BYTE UseType = UseType:Uses),BYTE,PROC,DERIVED
ValidateFieldServer    PROCEDURE(UNSIGNED Id,BYTE HandleErrors),BYTE,PROC,DERIVED
                     END


Hide:Relate:POPRAVLJA CLASS(RelationManager),TYPE          ! RelationManager for POPRAVLJA
Init                   PROCEDURE
DeferedAddRelations    PROCEDURE(),DERIVED
Kill                   PROCEDURE(),DERIVED
                     END

Hide:Access:CJENIK_USLUGA CLASS(FileManager),TYPE          ! FileManager for CJENIK_USLUGA
Init                   PROCEDURE(),DERIVED
Kill                   PROCEDURE(),DERIVED
UseFile                PROCEDURE(BYTE UseType = UseType:Uses),BYTE,PROC,DERIVED
                     END


Hide:Relate:CJENIK_USLUGA CLASS(RelationManager),TYPE      ! RelationManager for CJENIK_USLUGA
Init                   PROCEDURE
DeferedAddRelations    PROCEDURE(),DERIVED
Kill                   PROCEDURE(),DERIVED
                     END

Hide:Access:MJESTO   CLASS(FileManager),TYPE               ! FileManager for MJESTO
Init                   PROCEDURE(),DERIVED
Kill                   PROCEDURE(),DERIVED
UseFile                PROCEDURE(BYTE UseType = UseType:Uses),BYTE,PROC,DERIVED
                     END


Hide:Relate:MJESTO   CLASS(RelationManager),TYPE           ! RelationManager for MJESTO
Init                   PROCEDURE
DeferedAddRelations    PROCEDURE(),DERIVED
Kill                   PROCEDURE(),DERIVED
                     END

Hide:Access:KROJACNICA CLASS(FileManager),TYPE             ! FileManager for KROJACNICA
Init                   PROCEDURE(),DERIVED
Kill                   PROCEDURE(),DERIVED
UseFile                PROCEDURE(BYTE UseType = UseType:Uses),BYTE,PROC,DERIVED
ValidateFieldServer    PROCEDURE(UNSIGNED Id,BYTE HandleErrors),BYTE,PROC,DERIVED
                     END


Hide:Relate:KROJACNICA CLASS(RelationManager),TYPE         ! RelationManager for KROJACNICA
Init                   PROCEDURE
DeferedAddRelations    PROCEDURE(),DERIVED
Kill                   PROCEDURE(),DERIVED
                     END

Hide:Access:KROJAC   CLASS(FileManager),TYPE               ! FileManager for KROJAC
Init                   PROCEDURE(),DERIVED
Kill                   PROCEDURE(),DERIVED
PrimeFields            PROCEDURE(),PROC,DERIVED
UseFile                PROCEDURE(BYTE UseType = UseType:Uses),BYTE,PROC,DERIVED
ValidateFieldServer    PROCEDURE(UNSIGNED Id,BYTE HandleErrors),BYTE,PROC,DERIVED
                     END


Hide:Relate:KROJAC   CLASS(RelationManager),TYPE           ! RelationManager for KROJAC
Init                   PROCEDURE
DeferedAddRelations    PROCEDURE(),DERIVED
Kill                   PROCEDURE(),DERIVED
                     END


_Hide:Access:TKANINA &Hide:Access:TKANINA,AUTO,THREAD
_Hide:Relate:TKANINA &Hide:Relate:TKANINA,AUTO,THREAD
_Hide:Access:SIVA    &Hide:Access:SIVA,AUTO,THREAD
_Hide:Relate:SIVA    &Hide:Relate:SIVA,AUTO,THREAD
_Hide:Access:KLIJENT &Hide:Access:KLIJENT,AUTO,THREAD
_Hide:Relate:KLIJENT &Hide:Relate:KLIJENT,AUTO,THREAD
_Hide:Access:POPRAVLJA &Hide:Access:POPRAVLJA,AUTO,THREAD
_Hide:Relate:POPRAVLJA &Hide:Relate:POPRAVLJA,AUTO,THREAD
_Hide:Access:CJENIK_USLUGA &Hide:Access:CJENIK_USLUGA,AUTO,THREAD
_Hide:Relate:CJENIK_USLUGA &Hide:Relate:CJENIK_USLUGA,AUTO,THREAD
_Hide:Access:MJESTO  &Hide:Access:MJESTO,AUTO,THREAD
_Hide:Relate:MJESTO  &Hide:Relate:MJESTO,AUTO,THREAD
_Hide:Access:KROJACNICA &Hide:Access:KROJACNICA,AUTO,THREAD
_Hide:Relate:KROJACNICA &Hide:Relate:KROJACNICA,AUTO,THREAD
_Hide:Access:KROJAC  &Hide:Access:KROJAC,AUTO,THREAD
_Hide:Relate:KROJAC  &Hide:Relate:KROJAC,AUTO,THREAD


Krojacnica_BC0:DctInit PROCEDURE
  CODE
  _Hide:Access:TKANINA &= NEW(Hide:Access:TKANINA)
  _Hide:Relate:TKANINA &= NEW(Hide:Relate:TKANINA)
  _Hide:Access:SIVA &= NEW(Hide:Access:SIVA)
  _Hide:Relate:SIVA &= NEW(Hide:Relate:SIVA)
  _Hide:Access:KLIJENT &= NEW(Hide:Access:KLIJENT)
  _Hide:Relate:KLIJENT &= NEW(Hide:Relate:KLIJENT)
  _Hide:Access:POPRAVLJA &= NEW(Hide:Access:POPRAVLJA)
  _Hide:Relate:POPRAVLJA &= NEW(Hide:Relate:POPRAVLJA)
  _Hide:Access:CJENIK_USLUGA &= NEW(Hide:Access:CJENIK_USLUGA)
  _Hide:Relate:CJENIK_USLUGA &= NEW(Hide:Relate:CJENIK_USLUGA)
  _Hide:Access:MJESTO &= NEW(Hide:Access:MJESTO)
  _Hide:Relate:MJESTO &= NEW(Hide:Relate:MJESTO)
  _Hide:Access:KROJACNICA &= NEW(Hide:Access:KROJACNICA)
  _Hide:Relate:KROJACNICA &= NEW(Hide:Relate:KROJACNICA)
  _Hide:Access:KROJAC &= NEW(Hide:Access:KROJAC)
  _Hide:Relate:KROJAC &= NEW(Hide:Relate:KROJAC)
  Relate:TKANINA &= _Hide:Relate:TKANINA
  Relate:SIVA &= _Hide:Relate:SIVA
  Relate:KLIJENT &= _Hide:Relate:KLIJENT
  Relate:POPRAVLJA &= _Hide:Relate:POPRAVLJA
  Relate:CJENIK_USLUGA &= _Hide:Relate:CJENIK_USLUGA
  Relate:MJESTO &= _Hide:Relate:MJESTO
  Relate:KROJACNICA &= _Hide:Relate:KROJACNICA
  Relate:KROJAC &= _Hide:Relate:KROJAC


Krojacnica_BC0:FilesInit PROCEDURE
  CODE
  _Hide:Relate:TKANINA.Init
  _Hide:Relate:SIVA.Init
  _Hide:Relate:KLIJENT.Init
  _Hide:Relate:POPRAVLJA.Init
  _Hide:Relate:CJENIK_USLUGA.Init
  _Hide:Relate:MJESTO.Init
  _Hide:Relate:KROJACNICA.Init
  _Hide:Relate:KROJAC.Init


Krojacnica_BC0:DctKill PROCEDURE
  CODE
  _Hide:Relate:TKANINA.Kill
  DISPOSE(_Hide:Relate:TKANINA)
  _Hide:Relate:SIVA.Kill
  DISPOSE(_Hide:Relate:SIVA)
  _Hide:Relate:KLIJENT.Kill
  DISPOSE(_Hide:Relate:KLIJENT)
  _Hide:Relate:POPRAVLJA.Kill
  DISPOSE(_Hide:Relate:POPRAVLJA)
  _Hide:Relate:CJENIK_USLUGA.Kill
  DISPOSE(_Hide:Relate:CJENIK_USLUGA)
  _Hide:Relate:MJESTO.Kill
  DISPOSE(_Hide:Relate:MJESTO)
  _Hide:Relate:KROJACNICA.Kill
  DISPOSE(_Hide:Relate:KROJACNICA)
  _Hide:Relate:KROJAC.Kill
  DISPOSE(_Hide:Relate:KROJAC)


Hide:Relate:TKANINA.Init PROCEDURE
  CODE
  _Hide:Access:TKANINA.Init
  SELF.Init(Access:TKANINA,1)


Hide:Access:TKANINA.Init PROCEDURE

  CODE
  SELF.Initialized = False
  SELF.Buffer &= TKA:Record
  SELF.FileNameValue = 'TKANINA'
  SELF.SetErrors(GlobalErrors)
  SELF.File &= TKANINA
  PARENT.Init
  Access:TKANINA &= SELF


Hide:Access:TKANINA.Kill PROCEDURE

  CODE
  PARENT.Kill
  Access:TKANINA &= NULL                                   ! File manager has been destroyed to assign null, it is an error to reference this after this point


Hide:Access:TKANINA.UseFile PROCEDURE(BYTE UseType = UseType:Uses)

ReturnValue          BYTE,AUTO

  CODE
  IF UseType ~= UseType:Initialize
    SELF.UseFile(UseType:Initialize)                       !Recursive call to ensure initialization takes place
  END
  IF UseType = UseType:Initialize AND ~SELF.Initialized
    SELF.InUseFile = True
    SELF.Init(TKANINA,GlobalErrors)
    SELF.Create = 1
    SELF.LockRecover = 10
    SELF.AddKey(TKA:SK_Tkanina_NazivTkanine,'TKA:SK_Tkanina_NazivTkanine',0)
    SELF.AddKey(TKA:SK_Tkanina_BojaTkanine,'TKA:SK_Tkanina_BojaTkanine',0)
    SELF.AddKey(TKA:PK_Tkanina_SifraTkanine,'TKA:PK_Tkanina_SifraTkanine',0)
  END
                                                           !SELF.InUseFile will be set to False in PARENT
  ReturnValue = PARENT.UseFile(UseType)
  RETURN ReturnValue


Hide:Relate:TKANINA.DeferedAddRelations PROCEDURE

  CODE
  DO AddRelations_1
  
  PARENT.DeferedAddRelations

AddRelations_1 ROUTINE
  SELF.AddRelation(Relate:SIVA,RI:CASCADE,RI:CASCADE,SIV:VK_Siva_Tkanina_SifraTkanine)
  SELF.AddRelationLink(TKA:Sifra_tkanine,SIV:Sifra_tkanine)


Hide:Relate:TKANINA.Kill PROCEDURE

  CODE
  _Hide:Access:TKANINA.Kill                                ! Kill the file manager
  PARENT.Kill
  Relate:TKANINA &= NULL                                   ! Assign NULL to the RelationManager's FileManager reference, it is an error to reference this after this point
  DISPOSE(_Hide:Access:TKANINA)                            ! destroy the file manager


Hide:Relate:SIVA.Init PROCEDURE
  CODE
  _Hide:Access:SIVA.Init
  SELF.Init(Access:SIVA,1)


Hide:Access:SIVA.Init PROCEDURE

  CODE
  SELF.Initialized = False
  SELF.Buffer &= SIV:Record
  SELF.FileNameValue = 'SIVA'
  SELF.SetErrors(GlobalErrors)
  SELF.File &= SIVA
  PARENT.Init
  Access:SIVA &= SELF


Hide:Access:SIVA.Kill PROCEDURE

  CODE
  PARENT.Kill
  Access:SIVA &= NULL                                      ! File manager has been destroyed to assign null, it is an error to reference this after this point


Hide:Access:SIVA.PrimeFields PROCEDURE

  CODE
  SIV:Datum = TODAY()                                      ! Assign initial field value
  SIV:Popust = 0.15                                        ! Assign initial field value
  SIV:Vrijeme = CLOCK()                                    ! Assign initial field value
  PARENT.PrimeFields


Hide:Access:SIVA.UseFile PROCEDURE(BYTE UseType = UseType:Uses)

ReturnValue          BYTE,AUTO

  CODE
  IF UseType ~= UseType:Initialize
    SELF.UseFile(UseType:Initialize)                       !Recursive call to ensure initialization takes place
  END
  IF UseType = UseType:Initialize AND ~SELF.Initialized
    SELF.InUseFile = True
    SELF.Init(SIVA,GlobalErrors)
    SELF.Create = 1
    SELF.LockRecover = 10
    SELF.AddKey(SIV:VK_Siva_Tkanina_SifraTkanine,'SIV:VK_Siva_Tkanina_SifraTkanine',0)
    SELF.AddKey(SIV:PK_Siva_SifraKlijenta_SifraKrojaca_Datum,'SIV:PK_Siva_SifraKlijenta_SifraKrojaca_Datum',0)
    SELF.AddKey(SIV:RK_Siva_Datum_SifraKrojaca_SifraKlijenta,'SIV:RK_Siva_Datum_SifraKrojaca_SifraKlijenta',0)
  END
                                                           !SELF.InUseFile will be set to False in PARENT
  ReturnValue = PARENT.UseFile(UseType)
  RETURN ReturnValue


Hide:Access:SIVA.ValidateFieldServer PROCEDURE(UNSIGNED Id,BYTE HandleErrors)

ReturnValue          BYTE,AUTO

Save:TKANINA1      USHORT,AUTO
Save:KLIJENT2      USHORT,AUTO
Save:KROJAC3       USHORT,AUTO
  CODE
  ReturnValue = PARENT.ValidateFieldServer(Id,HandleErrors)
  CASE Id
  OF 1
    GlobalErrors.SetField('SIV:Sifra_tkanine')
    Save:TKANINA1 = Access:TKANINA.SaveFile()
    TKA:Sifra_tkanine = SIV:Sifra_tkanine
    ReturnValue = Access:TKANINA.TryFetch(TKA:PK_Tkanina_SifraTkanine)
    IF ReturnValue <> Level:Benign
      IF HandleErrors                                      ! Display message if handling errors
        ReturnValue = GlobalErrors.ThrowMessage(Msg:FieldNotInFile,'TKANINA')
      END
    END
    Access:TKANINA.RestoreFile(Save:TKANINA1)
  OF 2
    GlobalErrors.SetField('SIV:Sifra_klijenta')
    Save:KLIJENT2 = Access:KLIJENT.SaveFile()
    KLI:Sifra_klijenta = SIV:Sifra_klijenta
    ReturnValue = Access:KLIJENT.TryFetch(KLI:PK_Klijent_SifraKlijenta)
    IF ReturnValue <> Level:Benign
      IF HandleErrors                                      ! Display message if handling errors
        ReturnValue = GlobalErrors.ThrowMessage(Msg:FieldNotInFile,'KLIJENT')
      END
    END
    Access:KLIJENT.RestoreFile(Save:KLIJENT2)
  OF 3
    GlobalErrors.SetField('SIV:Sifra_krojaca')
    Save:KROJAC3 = Access:KROJAC.SaveFile()
    KRO:Sifra_krojaca = SIV:Sifra_krojaca
    ReturnValue = Access:KROJAC.TryFetch(KRO:PK_Krojac_SifraKrojaca)
    IF ReturnValue <> Level:Benign
      IF HandleErrors                                      ! Display message if handling errors
        ReturnValue = GlobalErrors.ThrowMessage(Msg:FieldNotInFile,'KROJAC')
      END
    END
    Access:KROJAC.RestoreFile(Save:KROJAC3)
  END
  RETURN ReturnValue


Hide:Relate:SIVA.DeferedAddRelations PROCEDURE

  CODE
  DO AddRelations_1
  
  PARENT.DeferedAddRelations

AddRelations_1 ROUTINE
  SELF.AddRelation(Relate:KROJAC)

  SELF.AddRelation(Relate:KLIJENT)

  SELF.AddRelation(Relate:TKANINA)


Hide:Relate:SIVA.Kill PROCEDURE

  CODE
  _Hide:Access:SIVA.Kill                                   ! Kill the file manager
  PARENT.Kill
  Relate:SIVA &= NULL                                      ! Assign NULL to the RelationManager's FileManager reference, it is an error to reference this after this point
  DISPOSE(_Hide:Access:SIVA)                               ! destroy the file manager


Hide:Relate:KLIJENT.Init PROCEDURE
  CODE
  _Hide:Access:KLIJENT.Init
  SELF.Init(Access:KLIJENT,1)


Hide:Access:KLIJENT.Init PROCEDURE

  CODE
  SELF.Initialized = False
  SELF.Buffer &= KLI:Record
  SELF.FileNameValue = 'KLIJENT'
  SELF.SetErrors(GlobalErrors)
  SELF.File &= KLIJENT
  PARENT.Init
  Access:KLIJENT &= SELF


Hide:Access:KLIJENT.Kill PROCEDURE

  CODE
  PARENT.Kill
  Access:KLIJENT &= NULL                                   ! File manager has been destroyed to assign null, it is an error to reference this after this point


Hide:Access:KLIJENT.UseFile PROCEDURE(BYTE UseType = UseType:Uses)

ReturnValue          BYTE,AUTO

  CODE
  IF UseType ~= UseType:Initialize
    SELF.UseFile(UseType:Initialize)                       !Recursive call to ensure initialization takes place
  END
  IF UseType = UseType:Initialize AND ~SELF.Initialized
    SELF.InUseFile = True
    SELF.Init(KLIJENT,GlobalErrors)
    SELF.Create = 1
    SELF.LockRecover = 10
    SELF.AddKey(KLI:PK_Klijent_SifraKlijenta,'KLI:PK_Klijent_SifraKlijenta',0)
  END
                                                           !SELF.InUseFile will be set to False in PARENT
  ReturnValue = PARENT.UseFile(UseType)
  RETURN ReturnValue


Hide:Relate:KLIJENT.DeferedAddRelations PROCEDURE

  CODE
  DO AddRelations_1
  
  PARENT.DeferedAddRelations

AddRelations_1 ROUTINE
  SELF.AddRelation(Relate:SIVA,RI:CASCADE,RI:CASCADE,SIV:PK_Siva_SifraKlijenta_SifraKrojaca_Datum)
  SELF.AddRelationLink(KLI:Sifra_klijenta,SIV:Sifra_klijenta)

  SELF.AddRelation(Relate:POPRAVLJA,RI:CASCADE,RI:CASCADE,POP:PK_Popravlja_SifraKlijenta_SifraUsluge_Datum)
  SELF.AddRelationLink(KLI:Sifra_klijenta,POP:Sifra_klijenta)


Hide:Relate:KLIJENT.Kill PROCEDURE

  CODE
  _Hide:Access:KLIJENT.Kill                                ! Kill the file manager
  PARENT.Kill
  Relate:KLIJENT &= NULL                                   ! Assign NULL to the RelationManager's FileManager reference, it is an error to reference this after this point
  DISPOSE(_Hide:Access:KLIJENT)                            ! destroy the file manager


Hide:Relate:POPRAVLJA.Init PROCEDURE
  CODE
  _Hide:Access:POPRAVLJA.Init
  SELF.Init(Access:POPRAVLJA,1)


Hide:Access:POPRAVLJA.Init PROCEDURE

  CODE
  SELF.Initialized = False
  SELF.Buffer &= POP:Record
  SELF.FileNameValue = 'POPRAVLJA'
  SELF.SetErrors(GlobalErrors)
  SELF.File &= POPRAVLJA
  PARENT.Init
  Access:POPRAVLJA &= SELF


Hide:Access:POPRAVLJA.Kill PROCEDURE

  CODE
  PARENT.Kill
  Access:POPRAVLJA &= NULL                                 ! File manager has been destroyed to assign null, it is an error to reference this after this point


Hide:Access:POPRAVLJA.PrimeFields PROCEDURE

  CODE
  POP:Datum = TODAY()                                      ! Assign initial field value
  POP:Vrijeme = CLOCK()                                    ! Assign initial field value
  PARENT.PrimeFields


Hide:Access:POPRAVLJA.UseFile PROCEDURE(BYTE UseType = UseType:Uses)

ReturnValue          BYTE,AUTO

  CODE
  IF UseType ~= UseType:Initialize
    SELF.UseFile(UseType:Initialize)                       !Recursive call to ensure initialization takes place
  END
  IF UseType = UseType:Initialize AND ~SELF.Initialized
    SELF.InUseFile = True
    SELF.Init(POPRAVLJA,GlobalErrors)
    SELF.Create = 1
    SELF.LockRecover = 10
    SELF.AddKey(POP:PK_Popravlja_SifraKlijenta_SifraUsluge_Datum,'POP:PK_Popravlja_SifraKlijenta_SifraUsluge_Datum',0)
    SELF.AddKey(POP:VK_Popravlja_Krojac_SifraKrojaca,'POP:VK_Popravlja_Krojac_SifraKrojaca',0)
  END
                                                           !SELF.InUseFile will be set to False in PARENT
  ReturnValue = PARENT.UseFile(UseType)
  RETURN ReturnValue


Hide:Access:POPRAVLJA.ValidateFieldServer PROCEDURE(UNSIGNED Id,BYTE HandleErrors)

ReturnValue          BYTE,AUTO

Save:KLIJENT1      USHORT,AUTO
Save:CJENIK_USLUGA3 USHORT,AUTO
Save:KROJAC7       USHORT,AUTO
  CODE
  ReturnValue = PARENT.ValidateFieldServer(Id,HandleErrors)
  CASE Id
  OF 1
    GlobalErrors.SetField('POP:Sifra_klijenta')
    Save:KLIJENT1 = Access:KLIJENT.SaveFile()
    KLI:Sifra_klijenta = POP:Sifra_klijenta
    ReturnValue = Access:KLIJENT.TryFetch(KLI:PK_Klijent_SifraKlijenta)
    IF ReturnValue <> Level:Benign
      IF HandleErrors                                      ! Display message if handling errors
        ReturnValue = GlobalErrors.ThrowMessage(Msg:FieldNotInFile,'KLIJENT')
      END
    END
    Access:KLIJENT.RestoreFile(Save:KLIJENT1)
  OF 3
    GlobalErrors.SetField('POP:Sifra_usluge')
    Save:CJENIK_USLUGA3 = Access:CJENIK_USLUGA.SaveFile()
    CJE:Sifra_usluge = POP:Sifra_usluge
    ReturnValue = Access:CJENIK_USLUGA.TryFetch(CJE:PK_Cjenik_SifraUsluge)
    IF ReturnValue <> Level:Benign
      IF HandleErrors                                      ! Display message if handling errors
        ReturnValue = GlobalErrors.ThrowMessage(Msg:FieldNotInFile,'CJENIK_USLUGA')
      END
    END
    Access:CJENIK_USLUGA.RestoreFile(Save:CJENIK_USLUGA3)
  OF 7
    GlobalErrors.SetField('POP:Sifra_krojaca')
    Save:KROJAC7 = Access:KROJAC.SaveFile()
    KRO:Sifra_krojaca = POP:Sifra_krojaca
    ReturnValue = Access:KROJAC.TryFetch(KRO:PK_Krojac_SifraKrojaca)
    IF ReturnValue <> Level:Benign
      IF HandleErrors                                      ! Display message if handling errors
        ReturnValue = GlobalErrors.ThrowMessage(Msg:FieldNotInFile,'KROJAC')
      END
    END
    Access:KROJAC.RestoreFile(Save:KROJAC7)
  END
  RETURN ReturnValue


Hide:Relate:POPRAVLJA.DeferedAddRelations PROCEDURE

  CODE
  DO AddRelations_1
  
  PARENT.DeferedAddRelations

AddRelations_1 ROUTINE
  SELF.AddRelation(Relate:KROJAC)

  SELF.AddRelation(Relate:KLIJENT)

  SELF.AddRelation(Relate:CJENIK_USLUGA)


Hide:Relate:POPRAVLJA.Kill PROCEDURE

  CODE
  _Hide:Access:POPRAVLJA.Kill                              ! Kill the file manager
  PARENT.Kill
  Relate:POPRAVLJA &= NULL                                 ! Assign NULL to the RelationManager's FileManager reference, it is an error to reference this after this point
  DISPOSE(_Hide:Access:POPRAVLJA)                          ! destroy the file manager


Hide:Relate:CJENIK_USLUGA.Init PROCEDURE
  CODE
  _Hide:Access:CJENIK_USLUGA.Init
  SELF.Init(Access:CJENIK_USLUGA,1)


Hide:Access:CJENIK_USLUGA.Init PROCEDURE

  CODE
  SELF.Initialized = False
  SELF.Buffer &= CJE:Record
  SELF.FileNameValue = 'CJENIK_USLUGA'
  SELF.SetErrors(GlobalErrors)
  SELF.File &= CJENIK_USLUGA
  PARENT.Init
  Access:CJENIK_USLUGA &= SELF


Hide:Access:CJENIK_USLUGA.Kill PROCEDURE

  CODE
  PARENT.Kill
  Access:CJENIK_USLUGA &= NULL                             ! File manager has been destroyed to assign null, it is an error to reference this after this point


Hide:Access:CJENIK_USLUGA.UseFile PROCEDURE(BYTE UseType = UseType:Uses)

ReturnValue          BYTE,AUTO

  CODE
  IF UseType ~= UseType:Initialize
    SELF.UseFile(UseType:Initialize)                       !Recursive call to ensure initialization takes place
  END
  IF UseType = UseType:Initialize AND ~SELF.Initialized
    SELF.InUseFile = True
    SELF.Init(CJENIK_USLUGA,GlobalErrors)
    SELF.Create = 1
    SELF.LockRecover = 10
    SELF.AddKey(CJE:PK_Cjenik_SifraUsluge,'CJE:PK_Cjenik_SifraUsluge',0)
  END
                                                           !SELF.InUseFile will be set to False in PARENT
  ReturnValue = PARENT.UseFile(UseType)
  RETURN ReturnValue


Hide:Relate:CJENIK_USLUGA.DeferedAddRelations PROCEDURE

  CODE
  DO AddRelations_1
  
  PARENT.DeferedAddRelations

AddRelations_1 ROUTINE
  SELF.AddRelation(Relate:POPRAVLJA,RI:CASCADE,RI:CASCADE,POP:PK_Popravlja_SifraKlijenta_SifraUsluge_Datum)
  SELF.AddRelationLink(CJE:Sifra_usluge,POP:Sifra_usluge)


Hide:Relate:CJENIK_USLUGA.Kill PROCEDURE

  CODE
  _Hide:Access:CJENIK_USLUGA.Kill                          ! Kill the file manager
  PARENT.Kill
  Relate:CJENIK_USLUGA &= NULL                             ! Assign NULL to the RelationManager's FileManager reference, it is an error to reference this after this point
  DISPOSE(_Hide:Access:CJENIK_USLUGA)                      ! destroy the file manager


Hide:Relate:MJESTO.Init PROCEDURE
  CODE
  _Hide:Access:MJESTO.Init
  SELF.Init(Access:MJESTO,1)


Hide:Access:MJESTO.Init PROCEDURE

  CODE
  SELF.Initialized = False
  SELF.Buffer &= MJE:Record
  SELF.FileNameValue = 'MJESTO'
  SELF.SetErrors(GlobalErrors)
  SELF.File &= MJESTO
  PARENT.Init
  Access:MJESTO &= SELF


Hide:Access:MJESTO.Kill PROCEDURE

  CODE
  PARENT.Kill
  Access:MJESTO &= NULL                                    ! File manager has been destroyed to assign null, it is an error to reference this after this point


Hide:Access:MJESTO.UseFile PROCEDURE(BYTE UseType = UseType:Uses)

ReturnValue          BYTE,AUTO

  CODE
  IF UseType ~= UseType:Initialize
    SELF.UseFile(UseType:Initialize)                       !Recursive call to ensure initialization takes place
  END
  IF UseType = UseType:Initialize AND ~SELF.Initialized
    SELF.InUseFile = True
    SELF.Init(MJESTO,GlobalErrors)
    SELF.Create = 1
    SELF.LockRecover = 10
    SELF.AddKey(MJE:PK_Mjesto_PostBr,'MJE:PK_Mjesto_PostBr',0)
  END
                                                           !SELF.InUseFile will be set to False in PARENT
  ReturnValue = PARENT.UseFile(UseType)
  RETURN ReturnValue


Hide:Relate:MJESTO.DeferedAddRelations PROCEDURE

  CODE
  DO AddRelations_1
  
  PARENT.DeferedAddRelations

AddRelations_1 ROUTINE
  SELF.AddRelation(Relate:KROJACNICA,RI:CASCADE,RI:RESTRICT,KROJ:VK_Krojacnica_Mjesto_PostBr)
  SELF.AddRelationLink(MJE:Postanski_broj,KROJ:Postanski_broj)


Hide:Relate:MJESTO.Kill PROCEDURE

  CODE
  _Hide:Access:MJESTO.Kill                                 ! Kill the file manager
  PARENT.Kill
  Relate:MJESTO &= NULL                                    ! Assign NULL to the RelationManager's FileManager reference, it is an error to reference this after this point
  DISPOSE(_Hide:Access:MJESTO)                             ! destroy the file manager


Hide:Relate:KROJACNICA.Init PROCEDURE
  CODE
  _Hide:Access:KROJACNICA.Init
  SELF.Init(Access:KROJACNICA,1)


Hide:Access:KROJACNICA.Init PROCEDURE

  CODE
  SELF.Initialized = False
  SELF.Buffer &= KROJ:Record
  SELF.FileNameValue = 'KROJACNICA'
  SELF.SetErrors(GlobalErrors)
  SELF.File &= KROJACNICA
  PARENT.Init
  Access:KROJACNICA &= SELF


Hide:Access:KROJACNICA.Kill PROCEDURE

  CODE
  PARENT.Kill
  Access:KROJACNICA &= NULL                                ! File manager has been destroyed to assign null, it is an error to reference this after this point


Hide:Access:KROJACNICA.UseFile PROCEDURE(BYTE UseType = UseType:Uses)

ReturnValue          BYTE,AUTO

  CODE
  IF UseType ~= UseType:Initialize
    SELF.UseFile(UseType:Initialize)                       !Recursive call to ensure initialization takes place
  END
  IF UseType = UseType:Initialize AND ~SELF.Initialized
    SELF.InUseFile = True
    SELF.Init(KROJACNICA,GlobalErrors)
    SELF.Create = 1
    SELF.LockRecover = 10
    SELF.AddKey(KROJ:SK_Krojacnica_NazivKrojacnice,'KROJ:SK_Krojacnica_NazivKrojacnice',0)
    SELF.AddKey(KROJ:PK_Krojacnica_SifraKrojacnice,'KROJ:PK_Krojacnica_SifraKrojacnice',0)
    SELF.AddKey(KROJ:VK_Krojacnica_Mjesto_PostBr,'KROJ:VK_Krojacnica_Mjesto_PostBr',0)
  END
                                                           !SELF.InUseFile will be set to False in PARENT
  ReturnValue = PARENT.UseFile(UseType)
  RETURN ReturnValue


Hide:Access:KROJACNICA.ValidateFieldServer PROCEDURE(UNSIGNED Id,BYTE HandleErrors)

ReturnValue          BYTE,AUTO

Save:MJESTO4       USHORT,AUTO
  CODE
  ReturnValue = PARENT.ValidateFieldServer(Id,HandleErrors)
  CASE Id
  OF 4
    GlobalErrors.SetField('KROJ:Postanski_broj')
    Save:MJESTO4 = Access:MJESTO.SaveFile()
    MJE:Postanski_broj = KROJ:Postanski_broj
    ReturnValue = Access:MJESTO.TryFetch(MJE:PK_Mjesto_PostBr)
    IF ReturnValue <> Level:Benign
      IF HandleErrors                                      ! Display message if handling errors
        ReturnValue = GlobalErrors.ThrowMessage(Msg:FieldNotInFile,'MJESTO')
      END
    END
    Access:MJESTO.RestoreFile(Save:MJESTO4)
  END
  RETURN ReturnValue


Hide:Relate:KROJACNICA.DeferedAddRelations PROCEDURE

  CODE
  DO AddRelations_1
  
  PARENT.DeferedAddRelations

AddRelations_1 ROUTINE
  SELF.AddRelation(Relate:KROJAC,RI:CASCADE,RI:RESTRICT,KRO:VK_Krojac_Krojacnica_SifraKrojacnice)
  SELF.AddRelationLink(KROJ:Sifra_krojacnice,KRO:Sifra_krojacnice)

  SELF.AddRelation(Relate:MJESTO)


Hide:Relate:KROJACNICA.Kill PROCEDURE

  CODE
  _Hide:Access:KROJACNICA.Kill                             ! Kill the file manager
  PARENT.Kill
  Relate:KROJACNICA &= NULL                                ! Assign NULL to the RelationManager's FileManager reference, it is an error to reference this after this point
  DISPOSE(_Hide:Access:KROJACNICA)                         ! destroy the file manager


Hide:Relate:KROJAC.Init PROCEDURE
  CODE
  _Hide:Access:KROJAC.Init
  SELF.Init(Access:KROJAC,1)


Hide:Access:KROJAC.Init PROCEDURE

  CODE
  SELF.Initialized = False
  SELF.Buffer &= KRO:Record
  SELF.FileNameValue = 'KROJAC'
  SELF.SetErrors(GlobalErrors)
  SELF.File &= KROJAC
  PARENT.Init
  Access:KROJAC &= SELF


Hide:Access:KROJAC.Kill PROCEDURE

  CODE
  PARENT.Kill
  Access:KROJAC &= NULL                                    ! File manager has been destroyed to assign null, it is an error to reference this after this point


Hide:Access:KROJAC.PrimeFields PROCEDURE

  CODE
  KRO:Datum_rodjenja = TODAY()                             ! Assign initial field value
  PARENT.PrimeFields


Hide:Access:KROJAC.UseFile PROCEDURE(BYTE UseType = UseType:Uses)

ReturnValue          BYTE,AUTO

  CODE
  IF UseType ~= UseType:Initialize
    SELF.UseFile(UseType:Initialize)                       !Recursive call to ensure initialization takes place
  END
  IF UseType = UseType:Initialize AND ~SELF.Initialized
    SELF.InUseFile = True
    SELF.Init(KROJAC,GlobalErrors)
    SELF.Create = 1
    SELF.LockRecover = 10
    SELF.AddKey(KRO:PK_Krojac_SifraKrojaca,'KRO:PK_Krojac_SifraKrojaca',0)
    SELF.AddKey(KRO:VK_Krojac_Krojacnica_SifraKrojacnice,'KRO:VK_Krojac_Krojacnica_SifraKrojacnice',0)
  END
                                                           !SELF.InUseFile will be set to False in PARENT
  ReturnValue = PARENT.UseFile(UseType)
  RETURN ReturnValue


Hide:Access:KROJAC.ValidateFieldServer PROCEDURE(UNSIGNED Id,BYTE HandleErrors)

ReturnValue          BYTE,AUTO

Save:KROJACNICA8   USHORT,AUTO
  CODE
  ReturnValue = PARENT.ValidateFieldServer(Id,HandleErrors)
  CASE Id
  OF 8
    GlobalErrors.SetField('KRO:Sifra_krojacnice')
    Save:KROJACNICA8 = Access:KROJACNICA.SaveFile()
    KROJ:Sifra_krojacnice = KRO:Sifra_krojacnice
    ReturnValue = Access:KROJACNICA.TryFetch(KROJ:PK_Krojacnica_SifraKrojacnice)
    IF ReturnValue <> Level:Benign
      IF HandleErrors                                      ! Display message if handling errors
        ReturnValue = GlobalErrors.ThrowMessage(Msg:FieldNotInFile,'KROJACNICA')
      END
    END
    Access:KROJACNICA.RestoreFile(Save:KROJACNICA8)
  END
  RETURN ReturnValue


Hide:Relate:KROJAC.DeferedAddRelations PROCEDURE

  CODE
  DO AddRelations_1
  
  PARENT.DeferedAddRelations

AddRelations_1 ROUTINE
  SELF.AddRelation(Relate:KROJACNICA)

  SELF.AddRelation(Relate:SIVA,RI:CASCADE,RI:CASCADE,SIV:RK_Siva_Datum_SifraKrojaca_SifraKlijenta)
  SELF.AddRelationLink(KRO:Sifra_krojaca,SIV:Sifra_krojaca)

  SELF.AddRelation(Relate:POPRAVLJA,RI:CASCADE,RI:CASCADE,POP:VK_Popravlja_Krojac_SifraKrojaca)
  SELF.AddRelationLink(KRO:Sifra_krojaca,POP:Sifra_krojaca)


Hide:Relate:KROJAC.Kill PROCEDURE

  CODE
  _Hide:Access:KROJAC.Kill                                 ! Kill the file manager
  PARENT.Kill
  Relate:KROJAC &= NULL                                    ! Assign NULL to the RelationManager's FileManager reference, it is an error to reference this after this point
  DISPOSE(_Hide:Access:KROJAC)                             ! destroy the file manager

