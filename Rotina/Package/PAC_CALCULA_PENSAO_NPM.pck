CREATE OR REPLACE PACKAGE USER_IPESP.PAC_CALCULA_PENSAO_NPM AS

   PERCENTAUXINVALIDEZ NUMBER;
   PAR_COD_INS         NUMBER(8);
   PAR_COD_USU         VARCHAR2(20);
   PAR_PAP_SEQ         NUMBER;
   PAR_TIP_TRA         CHAR;

   PAR_TRP_SEP         NUMBER(8);
   PAR_NUM_CPF         TB_PESSOA_FISICA.NUM_CPF%TYPE;
   PAR_ERR             NUMBER(8);
   PAR_DATA_OBITO      DATE;
   PAR_DATA_SOL        DATE; -- DATA DE SOLICITAC?O
   PAR_COD_TIPO_BEN    VARCHAR2(10);
   PAR_IDE_CLI         TB_PESSOA_FISICA.COD_IDE_CLI%TYPE;
   PAR_COD_IDE_CLI     VARCHAR2(20);
   PAR_NUM_MAT         VARCHAR2(20);
   PAR_IDE_PER         TB_PESSOA_FISICA.NUM_CPF%TYPE;
   PAR_IDE_EMP         VARCHAR2(14);
   PAR_IND_SIM_BEN     CHAR(1);
   PAR_COD_ENTIDADE    NUMBER(8);
   PAR_COD_CARGO       NUMBER(8);
   PAR_PCCS            NUMBER(8);
   PAR_SEXO            NUMBER;
   PAR_IDE_REL_FUNC    NUMBER(20);
   PAR_EH_INATIVO      NUMBER;
   PAR_COD_PCCS        NUMBER;
   PAR_DAT_PUBLICACAO  DATE;
   PAR_VAL_BENEFICIO   NUMBER(18,4);
   PAR_IND_TIP_CALC    CHAR(2);
   PAR_ORG_LEGADOR     CHAR(3);
   PAR_UO_LEGADOR      CHAR(3);
 
  /* Variaveis de calculo */
   PAR_COD_ADM_TRA              VARCHAR2(10);
   PAR_COD_TRA_COMPL            VARCHAR2(10);
   PAR_COD_TRA_COMPL2           VARCHAR2(10);
   PAR_DT_SOLICITACAO           DATE;
   PAR_TIP_BEN                  CHAR(6);
   PAR_REGRA_CALCULO            VARCHAR2(1);
   PAR_COD_BENEFICIO_APO        NUMBER(10);
   PAR_COD_BENEFICIO_APO_COMPL  NUMBER(10);
   PAR_COD_REGISTRO             NUMBER(8);
   PAR_COD_TAREFA               NUMBER(8);  
   PAR_COD_USER                 VARCHAR(20);
   PAR_DAT_ING_SERV_PUB         DATE;
   PAR_COD_ADM_CALC              VARCHAR2(10);
  /* Variaveis de Trabalho */

   EXISTE_BENEFICIO_APO      NUMBER;
   EXISTE_BENEFICIO_PENSAO  NUMBER;
  /* Variaveis de cursores */
  
    TYPE CURSOR_RUBRICA       IS REF CURSOR;
    TYPE CURSOR_AFASTAMENTO   IS REF CURSOR;
    TYPE CURSOR_SIMTRP        IS REF CURSOR;
    TYPE CURSOR_BENCAL        IS REF CURSOR;

  /*  Variaveis da Tabela de Regras  */
        GPAP_SEQ              NUMBER(8)  ; 
        GPAP_SEQBEN           NUMBER(8)  ;
        GPAP_cod_ins          NUMBER(8)  ;
        GPAP_cod_ide_cli_serv VARCHAR2(20)  ;
        GPAP_cod_adm_tra      VARCHAR2(10)  ;
        GPAP_FEC_PRO          DATE;
        GPAP_NUM_COR_PRO      NUMBER;
        GPAP_ind_tip_tra      CHAR(6)  ;
        GPAP_ind_tip_reg      VARCHAR2(1)  ;
        GPAP_ind_tip_cob      VARCHAR2(1)  ;
        GPAP_ind_pro_apr      VARCHAR2(1)  ;
        GPAP_tie_fal_ano      NUMBER(4)  ;
        GPAP_tie_fal_mes      NUMBER(4)  ;
        GPAP_tie_fal_dia      NUMBER(4)  ;
        GPAP_mon_por_pro      NUMBER(7,4)  ;
        GPAP_mon_est_pro      NUMBER(14,2)  ;
        GPAP_cod_car          NUMBER  ;
        GPAP_cod_fun          NUMBER  ;
        GPAP_cod_niv          NUMBER(8)  ;
        GPAP_ide_emp          VARCHAR2(15)  ;
        GPAP_ind_ace_ser      VARCHAR2(1)  ;
        GPAP_fec_ace_ser      DATE;
        GPAP_fec_ing          DATE;
        GPAP_fec_ult_man      DATE;
        GPAP_nom_usu_man      VARCHAR2(20)  ;
        GPAP_nom_pro_man      VARCHAR2(20)  ;
        GPAP_obs              VARCHAR2(4000);
        GPAP_tip_tra_fim      CHAR(6);
        GPAP_COD_BEN          NUMBER(8);
        GPAP_por_exced_teto   NUMBER(5,2);
        GPAP_val_teto         NUMBER(14,2);
        GPAP_val_exced_desc   NUMBER(14,2);
        GPAP_val_exced_teto   NUMBER(14,2);
        GPAP_val_ben_calc     NUMBER(14,2);
        GPAP_val_desc_calc    NUMBER(14,2);
        GPAP_por_retrib       NUMBER(7,4);
        GPAP_ind_tip_calc     VARCHAR2(1);
        GPAP_dat_invalidez    DATE;
        GPAP_num_laudo_medico NUMBER(15);
        CGPA_dat_obito        DATE;
        GPAP_NUM_MATRICULA    NUMBER  ;
        GPAP_COD_IDE_REL_FUNC NUMBER  ;
        GPAP_COD_ENTIDADE     NUMBER  ;
        CGPA_DAT_ING_SERV_PUB DATE;
        
  /* Variaveis de controle */
  oErrorCode           NUMBER ;
  oErrorMessage        VARCHAR2(500);

 /****** variaveis de Erro ****/
  v_erro_tipo0            exception;
  v_erro_tipo1            exception;
  v_erro_tipo2            exception;
  v_erro_tipo3            exception;
  v_erro_tipo4            exception;
  v_erro_tipo5            exception;
  v_erro_tipo6            exception;
  v_erro_tipo7            exception;
  v_erro                  exception;
 
  ---- Controle de sessao ---
   QTD_CONTROLE_TAREFA    NUMBER;

 PROCEDURE SP_ADM_DADOS_PENSAO
  (
         P_COD_INS     IN NUMBER       ,
         P_COD_ADM_TRA IN VARCHAR2     ,
         P_COD_REGISTRO IN NUMBER       ,
         P_COD_USER    IN VARCHAR      ,
         P_COD_ERRO    OUT NUMBER      ,
         P_MSG_ERRO    OUT VARCHAR2
  );
 
 
 FUNCTION SP_ATU_SIMTRP_DADOS_APOSENTADORIA (
                                               IN_TRP_SEP       IN NUMBER ,  
                                               IN_COD_ADM_TRA   IN VARCHAR,
                                               OERRORCODE       OUT NUMBER,
                                               OERRORMESSAGE    OUT VARCHAR 
                                              )RETURN NUMBER ;

 
 PROCEDURE SP_EMQUADRA_BENEFICIARIOS      ( P_COD_INS        IN  NUMBER ,
                                            P_COD_ADM_TRA    IN  VARCHAR,
                                            P_SEQ_BEN        IN  NUMBER ,
                                            P_COD_REGISTRO   IN  NUMBER ,
                                            OERRORCODE       OUT NUMBER ,
                                            OERRORMESSAGE    OUT VARCHAR
                                           );

Procedure SP_CALCULA_REGRA_PENSAO (
                                     IN_TRP_SEP       IN NUMBER ,  
                                     IN_COD_ADM_TRA   IN VARCHAR, 
                                     IN_COD_REGISTRO  IN NUMBER,
                                     OERRORCODE       OUT NUMBER,
                                     OERRORMESSAGE    OUT VARCHAR 
                                   );
  
 
procedure  SP_INCLUE_SIMPAP_PENSAO (
                          FGPAP_seq              IN NUMBER   ,
                          FGPAP_cod_ins          IN NUMBER   ,
                          FGPAP_cod_ide_cli_serv IN VARCHAR2 ,
                          FGPAP_cod_adm_tra      IN VARCHAR2 ,
                          FGPAP_FEC_PRO          IN DATE,
                          FGPAP_NUM_COR_PRO      IN NUMBER,
                          FGPAP_ind_tip_tra      IN CHAR  ,
                          FGPAP_ind_tip_reg      IN VARCHAR2  ,
                          FGPAP_ind_tip_cob      IN VARCHAR2  ,
                          FGPAP_ind_pro_apr      IN VARCHAR2 ,
                          FGPAP_tie_fal_ano      IN NUMBER   ,
                          FGPAP_tie_fal_mes      IN NUMBER   ,
                          FGPAP_tie_fal_dia      IN NUMBER   ,
                          FGPAP_mon_por_pro      IN NUMBER   ,
                          FGPAP_mon_est_pro      IN NUMBER   ,
                          FGPAP_cod_car          IN NUMBER   ,
                          FGPAP_cod_fun          IN NUMBER   ,
                          FGPAP_cod_niv          IN NUMBER   ,
                          FGPAP_ide_emp          IN VARCHAR2 ,
                          FGPAP_ind_ace_ser      IN VARCHAR2  ,
                          FGPAP_fec_ace_ser      IN DATE,
                          FGPAP_fec_ing          IN DATE,
                          FGPAP_fec_ult_man      IN DATE,
                          FGPAP_nom_usu_man      IN VARCHAR2 ,
                          FGPAP_nom_pro_man      IN VARCHAR2 ,
                          FGPAP_obs              IN VARCHAR2 ,
                          FGPAP_tip_tra_fim      IN CHAR     ,
                          FGPAP_COD_BEN          IN NUMBER   ,
                          FGPAP_por_exced_teto   IN NUMBER   ,
                          FGPAP_val_teto         IN NUMBER   ,
                          FGPAP_val_exced_desc   IN NUMBER   ,
                          FGPAP_val_exced_teto   IN NUMBER   ,
                          FGPAP_val_ben_calc     IN NUMBER   ,
                          FGPAP_val_desc_calc    IN NUMBER   ,
                          FGPAP_por_retrib       IN NUMBER   ,
                          FGPAP_ind_tip_calc     IN VARCHAR2 ,
                          FGPAP_dat_invalidez    IN DATE     ,
                          FGPAP_num_laudo_medico IN NUMBER   ,
                          p_flg_faleceu_atividade           IN  VARCHAR2, 
                          p_flg_obito_decorrente_exerc_raza IN  VARCHAR2, 
                          P_cod_beneficio_faleceu_atividade IN  NUMBER  ,
                          P_flg_faleceu_acid_doenca         IN  VARCHAR2,                               
                          
                          P_ERRORCODE           OUT NUMBER  ,
                          P_MENSAJE             OUT VARCHAR2);

  PROCEDURE SP_LIMPA_ADM_DADOS_PENSAO
  (
         P_COD_INS     IN NUMBER       ,
         P_COD_ADM_TRA IN VARCHAR2     ,
         P_COD_REGITRO IN NUMBER       ,
         P_COD_ERRO    OUT NUMBER      ,
         P_MSG_ERRO    OUT VARCHAR2
  );
   PROCEDURE SP_LIMPA_ADM_DADOS_RUBRICA
  (
         P_COD_INS       IN NUMBER       ,
         P_COD_ADM_TRA   IN VARCHAR2     ,
         P_COD_REGITRO   IN NUMBER       ,
         P_IND_TIP_CALC  IN CHAR         ,
         P_COD_ERRO      OUT NUMBER      ,
         P_MSG_ERRO      OUT VARCHAR2
  ); 
 
 
 ------ Procedimentos de Obtencao do dados para pagamento,
   Procedure SP_OBTEM_DADOS_APOSENTADORIA_REGRA1
                                     (P_COD_INS                         IN NUMBER    ,
                                      P_COD_ADM_TRA                     IN VARCHAR   ,
                                      P_COD_BENEFICIO_APO               IN NUMBER    ,                                
                                      P_IND_TIP_COB                     OUT VARCHAR  ,
                                      P_POR_RETRIB                      OUT NUMBER   ,
                                      OERRORCODE                        OUT NUMBER   ,
                                      OERRORMESSAGE                     OUT VARCHAR);
                                      
    Procedure SP_OBTEM_DADOS_APOSENTADORIA_REGRA2
                                     (P_COD_INS        IN NUMBER ,
                                      P_COD_ADM_TRA    IN VARCHAR,
                                      OERRORCODE       OUT NUMBER,
                                      OERRORMESSAGE    OUT VARCHAR);
 
   Procedure SP_OBTEM_DADOS_APOSENTADORIA_REGRA3
                                     (P_COD_INS             IN  NUMBER  ,
                                      P_COD_ADM_TRA         IN  VARCHAR ,
                                      P_DAT_CALC            IN  DATE    ,
                                      P_COD_IDE_CLI_SERV    IN  VARCHAR2,
                                      P_NUM_MATRICULA       IN  NUMBER  ,
                                      P_COD_IDE_REL_FUNC    IN  NUMBER  ,
                                      P_COD_ENTIDADE        IN  NUMBER  ,
                                      P_QTD_POR_SALARIOS    IN  NUMBER  ,
                                      P_TIP_CALCULO         IN  VARCHAR ,
                                      P_POR_CALCULO         IN  NUMBER  ,
                                      P_DAT_ING_SERV_PUB    IN  DATE    ,
                                      P_COD_TAREFA          IN  NUMBER  ,
                                      P_COD_REGISTRO        IN  NUMBER  ,
                                      P_COD_USER            IN VARCHAR  , 
                                      P_SEQBEN              IN NUMBER   ,                                                                           
                                      OERRORCODE            OUT NUMBER  ,
                                      OERRORMESSAGE         OUT VARCHAR);  
 
 
 PROCEDURE SP_REGISTRA_DADOS_PENSAO
  (
         P_COD_INS      IN NUMBER       ,
         P_COD_ADM_TRA  IN VARCHAR2     ,
         P_COD_REGISTRO IN NUMBER       ,
         P_COD_USER     IN VARCHAR      ,
         P_COD_ERRO    OUT NUMBER       ,
         P_MSG_ERRO    OUT VARCHAR2
  );
 
 PROCEDURE SP_REGISTRA_DADOS_UPD (P_COD_INS                IN NUMBER   ,
                                  P_COD_ADM_TRA            IN VARCHAR2 ,
                                  P_IND_TIP_CALC           IN NUMBER   ,
                                  P_COD_USER               IN VARCHAR2 ,
                                  P_COD_ENTIDADE           IN NUMBER   ,
                                  P_DAT_ING_SERV_PUB       IN DATE     ,
                                  P_DAT_CALC               IN DATE     ,
                                  P_MAX_NUM_SEQ            IN NUMBER   ,
                                  P_COD_ADM_TRA_PRINC      IN VARCHAR2 ,
                                  OERRORCODE       OUT NUMBER    ,
                                  OERRORMESSAGE    OUT VARCHAR   ); 
 
  FUNCTION SP_VALIDA_TETO_COMPLEMETAR ( 
                                  P_COD_ENTIDADE     IN NUMBER   ,
                                  P_DAT_ING_SERV_PUB IN DATE    
                                 ) RETURN BOOLEAN ;
  FUNCTION SP_OBTEM_VARIAVEL ( 
                              P_COD_PARAM      IN VARCHAR,
                              P_COD_ESTRUTURA  IN NUMBER ,
                              P_COD_ELEMENTO   IN VARCHAR,
                              P_DAT_CALCULO    IN  DATE 
                              ) RETURN NUMBER;
                              
  PROCEDURE SP_COMPOSICAO_BASE(
                          P_COD_INS        IN  NUMBER  ,
                          P_COD_RUBRICA    IN  NUMBER  ,
                          P_VARIAVEL       IN  VARCHAR2,
                          P_COD_ADM_TRA    IN  NUMBER  ,
                          P_COD_ENTIDADE   IN  NUMBER  ,
                          P_PAR_PER_PRO    IN  DATE    ,
                          O_VALOR          OUT NUMBER  ,
                          OERRORCODE       OUT NUMBER  ,
                          OERRORMESSAGE    OUT VARCHAR                                
                          );
   FUNCTION SP_OBTEM_ACUMUATIVIDADE ( 
                              P_DAT_CALCULO      IN  DATE   ,
                              P_VALOR_BENEFICIO  IN  NUMBER , 
                              OERRORCODE         OUT NUMBER ,
                             OERRORMESSAGE      OUT VARCHAR                                
   ) RETURN NUMBER;
   
   PROCEDURE SP_FORMA_PENSAO (
         P_COD_INS      IN NUMBER       ,
         P_COD_ADM_TRA  IN VARCHAR2     ,
         P_COD_REGISTRO IN NUMBER       ,
         P_COD_USER     IN VARCHAR      ,
         P_COD_ERRO    OUT NUMBER      ,
         P_MSG_ERRO    OUT VARCHAR2  
    );
   PROCEDURE  SP_RECALCULA_COMPSALTRA (
         P_COD_INS      IN NUMBER       ,
         P_COD_ADM_TRA  IN VARCHAR2     ,
         P_COD_REGISTRO IN NUMBER       ,
         P_COD_USER     IN VARCHAR      ,
         P_COD_ERRO    OUT NUMBER       ,
         P_MSG_ERRO    OUT VARCHAR2  
    ) ;   
    
  PROCEDURE SP_CALCULA_MEDIA_PENSAO
  (
         P_COD_INS              IN NUMBER       ,
         P_COD_ADM_TRA_VINCULO  IN VARCHAR2     ,
         P_COD_REGISTRO         IN NUMBER       ,   
         P_COD_USER             IN VARCHAR      ,
         P_COD_ERRO             OUT NUMBER      ,
         P_MSG_ERRO             OUT VARCHAR2
  );   
 FUNCTION SP_OBTEM_RUBRICA_LEI1354 ( 
                                  P_COD_INS          IN NUMBER   ,
                                  P_BOL_GERA_REDUTOR IN CHAR     
                                 ) RETURN NUMBER;
  
 FUNCTION SP_OBTEM_TETO_RGPS (P_DATA_OBITO DATE)    RETURN NUMBER ; 
 
 PROCEDURE  SP_GRAVA_RESERVA_COTA(P_COD_INS           IN  TB_NPM_BENEFICIARIO.COD_INS%TYPE
                                 ,P_COD_ADM_TRA       IN  TB_NPM_BENEFICIARIO.COD_ADM_TRA%TYPE 
                                 ,P_COD_IDE_CLI_SERV  IN  TB_NPM_BENEFICIARIO.COD_IDE_CLI_SERV%TYPE
                                 ,P_COD_BENEFICIO     IN  TB_BENEFICIARIO.COD_BENEFICIO%TYPE
                                 ,P_USU_PROC          IN  TB_BENEFICIO_RATEIO_PENSAO.NOM_USU_ULT_ATU%TYPE 
                                 ,P_COD_ERRO          OUT NUMBER
                                 ,P_MSG_ERRO          OUT VARCHAR2); 
                                   

  PROCEDURE SP_INCL_IMPORTA_CALCULO (P_COD_INS           IN  TB_NPM_BENEFICIARIO.COD_INS%TYPE
                                   ,P_COD_ADM_TRA       IN  TB_NPM_BENEFICIARIO.COD_ADM_TRA%TYPE 
                                   ,P_USU_PROC          IN  TB_BENEFICIO_RATEIO_PENSAO.NOM_USU_ULT_ATU%TYPE 
                                   ,P_COD_ERRO          OUT NUMBER
                                   ,P_MSG_ERRO          OUT VARCHAR2);
END PAC_CALCULA_PENSAO_NPM;
/
CREATE OR REPLACE PACKAGE BODY USER_IPESP.PAC_CALCULA_PENSAO_NPM AS
                                        
/*******************************************************************
    Nome        :  PAC_CALCULA_PENSAO_NPM
    Tipo        : Stored Procedure
    Descricao   : Realiza o calculo dos anos trabalhados segundo a Nova lei de reforma
                  da previdencia 1354/2020
    Autor       : Atlantic Soultions
    Data        : 02/05/2020
    Modificado por  :
    Modificado em   :
*********************************************************************/

   PROCEDURE SP_ADM_DADOS_PENSAO
  (
         P_COD_INS      IN NUMBER       ,
         P_COD_ADM_TRA  IN VARCHAR2     ,
         P_COD_REGISTRO IN NUMBER       ,
         P_COD_USER     IN VARCHAR      ,
         P_COD_ERRO    OUT NUMBER      ,
         P_MSG_ERRO    OUT VARCHAR2
  ) IS

     V_CONT NUMBER;
     V_CPF VARCHAR2(11);
     V_TIPO_CALCULO VARCHAR2(10) := 'SIMULACAO';

  BEGIN
    BEGIN
        EXECUTE IMMEDIATE 'alter session set nls_date_format=''dd/mm/yyyy''';
 
      PAR_COD_ADM_TRA     := '';
      PAR_COD_TRA_COMPL   := null ;
      PAR_COD_INS         :=P_COD_INS;
      PAR_COD_REGISTRO    :=P_COD_REGISTRO; 
      PAR_COD_USER        :=P_COD_USER;
 
 
       
      IF NVL (OERRORCODE,0)  >0  THEN
          RAISE  v_erro_tipo0;
      END IF;    
      
  
/*        SELECT    COUNT(*)  
            INTO QTD_CONTROLE_TAREFA 
             FROM WRKREG P
            WHERE P.REG_COD_INS     =   P_COD_INS     AND 
                  P.REG_COD_TIPO    =   'NPM'          AND
                  P.REG_COD_REGISTRO=   P_COD_REGISTRO AND
                  P.REG_COD_ESTADO  =   'T'            AND
                  P.REG_COD_ADM_TRA =   P_COD_ADM_TRA;
                  
         IF  QTD_CONTROLE_TAREFA  <> 0 THEN 
             RETURN;
         END IF;*/
               
  
  
      
     BEGIN
       SELECT
             S.TRP_SEQ          ,
             S.TRP_FEC_SOL_TRA  ,
             S.TRP_IDE_CLI      ,
             S.TRP_IND_TIP_TRA  ,
             S.TRP_COD_ADM_TRA  ,
             S.TRP_IDE_PER      ,
             S.TRP_NUM_MAT      ,
             S.TRP_COD_TRA_COMPL,
             S.TRP_IDE_REL_FUNC ,
             S.TRP_COD_ENTIDADE ,
             S.TRP_CARGO_PEN    ,
             SER.COD_TAREFA
        INTO PAR_TRP_SEP        ,
             PAR_DT_SOLICITACAO ,
             PAR_COD_IDE_CLI    ,
             PAR_TIP_BEN        ,
             PAR_COD_ADM_TRA    ,
             PAR_IDE_PER        ,
             PAR_NUM_MAT        ,
             PAR_COD_TRA_COMPL  ,
             PAR_IDE_REL_FUNC   ,
             PAR_COD_ENTIDADE   ,
             PAR_COD_CARGO      ,
             PAR_COD_TAREFA    
      FROM SIMTRP S, TB_NPM_SERVIDOR SER
      WHERE S.TRP_COD_ADM_TRA  = P_COD_ADM_TRA
        AND S.TRP_COD_INS      = P_COD_INS
        AND SER.COD_INS        = P_COD_INS
        AND SER.COD_ADM_TRA    = P_COD_ADM_TRA
        AND SER.COD_REGISTRO   = P_COD_REGISTRO;
      EXCEPTION
       When OTHERS Then
        RAISE  v_erro_tipo1;
      END;

   

    ----- Verifica se existe processo de Pens?o associaso ao Viculo ---
      IF PAR_TIP_BEN <> 'NPI' THEN 
        BEGIN
          
           SELECT COUNT(*)
           INTO EXISTE_BENEFICIO_PENSAO
            FROM TB_CONCESSAO_BENEFICIO CC
           WHERE CC.COD_INS             = PAR_COD_INS
            AND  CC.COD_IDE_CLI_SERV    = PAR_COD_IDE_CLI
            AND  CC.COD_ENTIDADE        = PAR_COD_ENTIDADE
            AND  CC.NUM_MATRICULA       = PAR_NUM_MAT
            AND  CC.COD_IDE_REL_FUNC    = PAR_IDE_REL_FUNC
            AND  CC.COD_TIPO_BENEFICIO  ='M';

        END;
         IF EXISTE_BENEFICIO_PENSAO > 0 THEN
               RETURN;
            -- RAISE  v_erro_tipo2;
        END IF;
     END IF;  
     

        ---- Erro em limpeza de dados ----
           SP_LIMPA_ADM_DADOS_PENSAO
                  (
                         PAR_COD_INS      ,
                         PAR_COD_ADM_TRA  ,
                         PAR_COD_REGISTRO ,
                         OERRORCODE       ,
                         OERRORMESSAGE    
                  );

    ----- Verifica se existe processo de Aposentadoria associaso ao Viculo ---
      BEGIN
         SELECT COUNT(*) , MAX(CC.COD_BENEFICIO) 
         INTO EXISTE_BENEFICIO_APO , PAR_COD_BENEFICIO_APO
          FROM TB_CONCESSAO_BENEFICIO CC, TB_BENEFICIARIO BB
         WHERE CC.COD_INS             = PAR_COD_INS
          AND  CC.COD_IDE_CLI_SERV    = PAR_COD_IDE_CLI
          AND  CC.COD_ENTIDADE        = PAR_COD_ENTIDADE
          AND  CC.NUM_MATRICULA       = PAR_NUM_MAT
          AND  CC.COD_IDE_REL_FUNC    = PAR_IDE_REL_FUNC
          AND  CC.COD_TIPO_BENEFICIO !='M'
          --- Incluido em 12-02-2020
          AND  BB.COD_INS           = PAR_COD_INS
          AND  BB.COD_BENEFICIO    =  CC.COD_BENEFICIO
          AND  BB.COD_PROC_GRP_PAG <> 82 ;

      END;

      IF EXISTE_BENEFICIO_APO >0   THEN
        --- Exposta calculo do vinculo existente  ----
          IF  PAR_COD_BENEFICIO_APO >= 80000000  THEN
            
               EXISTE_BENEFICIO_APO:= SP_ATU_SIMTRP_DADOS_APOSENTADORIA (
                                                  P_COD_INS      ,
                                                  P_COD_ADM_TRA  ,
                                                  oErrorCode     ,
                                                  oErrorMessage
                                                        );
               IF  oErrorCode !=0 THEN 
                   RAISE  v_erro_tipo3;
               END IF;
          END IF;
      ELSE
     ----- Calculo de Tempo do Vinculo Principal -----
          --- Comentado para n?o realizar calculo de Tempo  
           NULL;
         /*
                 PAC_CALCULA_TEMPO_VATC.sp_vatc
                 (
                       P_COD_INS      ,
                       P_COD_ADM_TRA  ,
                       P_MSG_ERRO
                  );
                  IF P_MSG_ERRO IS NOT NULL THEN
                    RAISE  v_erro_tipo4;
                  END IF;
          */
     END IF;
     ----- Identifica a regra correspondente a Pens?o --       
     SP_CALCULA_REGRA_PENSAO ( PAR_TRP_SEP     ,
                               PAR_COD_ADM_TRA ,
                               PAR_COD_REGISTRO,
                               P_COD_ERRO      ,
                               P_MSG_ERRO );
     IF PAR_TIP_BEN = 'NPI' THEN      
       SP_INCL_IMPORTA_CALCULO (P_COD_INS,
                                P_COD_ADM_TRA,   
                                P_COD_USER,
                                P_COD_ERRO, 
                                P_MSG_ERRO);        
       
     END IF;
     

     IF  PAR_COD_TRA_COMPL IS NOT NULL THEN
       ----- Calculo de Tempo do Vinculo Secundario -----
           BEGIN
             SELECT
                     S.TRP_SEQ          ,
                     S.TRP_FEC_SOL_TRA  ,
                     S.TRP_IDE_CLI      ,
                     S.TRP_IND_TIP_TRA  ,
                     S.TRP_COD_ADM_TRA  ,
                     S.TRP_IDE_PER      ,
                     S.TRP_NUM_MAT      ,
                     S.TRP_COD_TRA_COMPL,
                     S.TRP_IDE_REL_FUNC ,
                     S.TRP_COD_ENTIDADE ,
                     S.TRP_CARGO_PEN
                INTO PAR_TRP_SEP        ,
                     PAR_DT_SOLICITACAO ,
                     PAR_COD_IDE_CLI    ,
                     PAR_TIP_BEN        ,
                     PAR_COD_ADM_TRA    ,
                     PAR_IDE_PER        ,
                     PAR_NUM_MAT        ,
                     PAR_COD_TRA_COMPL2 ,
                     PAR_IDE_REL_FUNC   ,
                     PAR_COD_ENTIDADE   ,
                     PAR_COD_CARGO
              FROM SIMTRP S
              WHERE S.TRP_COD_ADM_TRA  = PAR_COD_TRA_COMPL
                AND S.TRP_COD_INS      = P_COD_INS ;
              EXCEPTION
               When OTHERS Then
                RAISE  v_erro_tipo5;
              END;


          ---- Erro em limpeza de dados ----
           SP_LIMPA_ADM_DADOS_PENSAO
                  (
                         PAR_COD_INS      ,
                         PAR_COD_TRA_COMPL  ,
                         PAR_COD_REGISTRO ,
                         OERRORCODE       ,
                         OERRORMESSAGE    
                  );




          ----- Verifica se existe processo de Aposentadoria associaso ao Viculo ---
              BEGIN
                
                 SELECT COUNT(*) , MAX(CC.COD_BENEFICIO) 
                 INTO EXISTE_BENEFICIO_APO , PAR_COD_BENEFICIO_APO_COMPL
                  FROM TB_CONCESSAO_BENEFICIO CC
                 WHERE CC.COD_INS             = PAR_COD_INS
                  AND  CC.COD_IDE_CLI_SERV    = PAR_COD_IDE_CLI
                  AND  CC.COD_ENTIDADE        = PAR_COD_ENTIDADE
                  AND  CC.NUM_MATRICULA       = PAR_NUM_MAT
                  AND  CC.COD_IDE_REL_FUNC    = PAR_IDE_REL_FUNC
                  AND  CC.COD_TIPO_BENEFICIO !='M';

              END;

              IF EXISTE_BENEFICIO_APO >0  THEN
                    --- Exposta calculo do vinculo existente  ----
                     IF  PAR_COD_BENEFICIO_APO_COMPL >= 80000000  THEN
     
                           EXISTE_BENEFICIO_APO:= SP_ATU_SIMTRP_DADOS_APOSENTADORIA 
                                                     (P_COD_INS      ,
                                                      PAR_COD_TRA_COMPL ,
                                                      oErrorCode     ,
                                                      oErrorMessage
                                                              );
                         IF  oErrorCode !=0 THEN 
                             RAISE  v_erro_tipo3;
                         END IF;
                   
                      END IF;

                ELSE
                 ----- Calculo de Tempo do Vinculo Principal -----
                    --- Comentado para n?o realizar calculo de Tempo  
                     NULL;
                   /*
                       PAC_CALCULA_TEMPO_VATC.sp_vatc
                       (
                             P_COD_INS      ,
                             PAR_COD_TRA_COMPL  ,
                             P_MSG_ERRO
                        );
                        IF P_MSG_ERRO IS NOT NULL THEN
                          RAISE  v_erro_tipo4;
                        END IF;
                    */
              END IF;
                ----- Identifica a regra correspondente a Pens?o -- 
               SP_CALCULA_REGRA_PENSAO ( PAR_TRP_SEP     ,
                                         PAR_COD_TRA_COMPL ,
                                         PAR_COD_REGISTRO,
                                         P_COD_ERRO      ,
                                         P_MSG_ERRO );
                  
              
      END IF;




    END;

    EXCEPTION
      WHEN v_erro_tipo0 THEN
        P_COD_ERRO    := 01;
        P_MSG_ERRO    :=  OERRORMESSAGE;

      WHEN v_erro_tipo1 THEN
        P_COD_ERRO    := 02;
        P_MSG_ERRO    := 'Nao existe informacao do Tramite';

      WHEN v_erro_tipo2 THEN
        P_COD_ERRO    := 03;
        P_MSG_ERRO    := 'Erro no calculo do Vinculo principal Existe beneficio de Pens?o'||'- ('|| PAR_COD_ADM_TRA||')'  ;


     WHEN v_erro_tipo3 THEN
        P_COD_ERRO    := 04;
        P_MSG_ERRO    := 'Erro aposentodoria origem n?o identificada'||'- ('|| PAR_COD_ADM_TRA||')'  ;


      WHEN v_erro_tipo4 THEN
        P_COD_ERRO    := 05;
        P_MSG_ERRO    := 'Erro no calculo do Vinculo principal'||'- ('|| PAR_COD_ADM_TRA||')'  ;



      WHEN v_erro_tipo5 THEN
        P_COD_ERRO    := 05;
        P_MSG_ERRO    := 'Nao existe informacao do Tramite complementar'||'- ('|| PAR_COD_TRA_COMPL ||')'  ;


      WHEN OTHERS THEN
        P_COD_ERRO    := 99;
        P_MSG_ERRO    := SQLERRM;



   END;
 
  FUNCTION SP_ATU_SIMTRP_DADOS_APOSENTADORIA (
                                               IN_TRP_SEP       IN NUMBER ,  
                                               IN_COD_ADM_TRA   IN VARCHAR,
                                               OERRORCODE       OUT NUMBER,
                                               OERRORMESSAGE    OUT VARCHAR 
                                              ) RETURN NUMBER  AS

              OLD_dia_con_reg_ant       SIMTRP.trp_dia_con_reg_ant%TYPE;
              OLD_dia_con_reg_nue       SIMTRP.trp_dia_con_reg_nue%TYPE;
              OLD_tot_dia_con           SIMTRP.trp_tot_dia_con%TYPE;
              OLD_dia_exi_pen_int       SIMTRP.trp_dia_exi_pen_int%TYPE;
              OLD_dia_fal_pen_int       SIMTRP.trp_dia_fal_pen_int%TYPE;
              OLD_ind_inv_may           SIMTRP.trp_ind_inv_may%TYPE;
              OLD_fec_dic_inv           SIMTRP.trp_fec_dic_inv%TYPE;
              OLD_fec_ini_incap         SIMTRP.trp_fec_ini_incap%TYPE;
              OLD_fec_est_parto         SIMTRP.trp_fec_est_parto%TYPE;
              OLD_ind_exi_abo           SIMTRP.trp_ind_exi_abo%TYPE;
              OLD_ind_fal_ina           SIMTRP.trp_ind_fal_ina%TYPE;
              OLD_fec_fal               SIMTRP.trp_fec_fal%TYPE;
              OLD_fec_reclu             SIMTRP.trp_fec_reclu%TYPE;
              OLD_ley_reg_ant           SIMTRP.trp_ley_reg_ant%TYPE;
              OLD_reg_jur_ant           SIMTRP.trp_reg_jur_ant%TYPE;
              OLD_lic_pre_dia           SIMTRP.trp_lic_pre_dia%TYPE;
              OLD_ind_pro_dis           SIMTRP.trp_ind_pro_dis%TYPE;
              OLD_ind_deb_era           SIMTRP.trp_ind_deb_era%TYPE;
              OLD_tie_ser_pol           SIMTRP.trp_tie_ser_pol%TYPE;
              OLD_dia_mag_ant           SIMTRP.trp_dia_mag_ant%TYPE;
              OLD_dia_mag_nue           SIMTRP.trp_dia_mag_nue%TYPE;
              OLD_fec_sol_pub           SIMTRP.trp_fec_sol_pub%TYPE;
              OLD_fec_ini_pub           SIMTRP.trp_fec_ini_pub%TYPE;
              OLD_fec_fin_pub           SIMTRP.trp_fec_fin_pub%TYPE;
              OLD_fec_exp               SIMTRP.trp_fec_exp%TYPE;
              OLD_cod_est               SIMTRP.trp_cod_est%TYPE;
              OLD_cod_sec               SIMTRP.trp_cod_sec%TYPE;
              OLD_entrada_tramite       SIMTRP.trp_entrada_tramite%TYPE;
              OLD_dia_con_98_03         SIMTRP.trp_dia_con_98_03%TYPE;
              OLD_dia_carreira          SIMTRP.trp_dia_carreira%TYPE;
              OLD_cod_ini_tra           SIMTRP.trp_cod_ini_tra%TYPE;
              OLD_invalidez_integral    SIMTRP.trp_invalidez_integral%TYPE;
              OLD_dia_averbado          SIMTRP.trp_dia_averbado%TYPE;
              OLD_dat_ped_20            SIMTRP.trp_dat_ped_20%TYPE;
              OLD_dat_ped_40            SIMTRP.trp_dat_ped_40%TYPE;
              OLD_ind_sim_ben           SIMTRP.trp_ind_sim_ben%TYPE;
              OLD_tip_tra_fim           SIMTRP.trp_tip_tra_fim%TYPE;
              OLD_cod_ben               SIMTRP.trp_cod_ben%TYPE;
              OLD_ind_par               SIMTRP.trp_ind_par%TYPE;
              OLD_status                SIMTRP.trp_status%TYPE;
              OLD_obs                   SIMTRP.trp_obs%TYPE;
              OLD_tip_contrib           SIMTRP.trp_tip_contrib%TYPE;
              OLD_cod_mot_deslig        SIMTRP.trp_cod_mot_deslig%TYPE;
              OLD_ind_prefeito          SIMTRP.trp_ind_prefeito%TYPE;
              OLD_ide_rel_func          SIMTRP.trp_ide_rel_func%TYPE;
              OLD_cod_tra_compl         SIMTRP.trp_cod_tra_compl%TYPE;
              OLD_cod_entidade          SIMTRP.trp_cod_entidade%TYPE;
              OLD_ind_acid_tra          SIMTRP.trp_ind_acid_tra%TYPE;
              OLD_excl_magist_docente   SIMTRP.trp_excl_magist_docente%TYPE;
              OLD_qtd_faltas_desc_apo   SIMTRP.trp_qtd_faltas_desc_apo%TYPE;
              OLD_status_proc_tec       SIMTRP.trp_status_proc_tec%TYPE;
              OLD_obs_proc_tec          SIMTRP.trp_obs_proc_tec%TYPE;
              OLD_status_conf_proc_sup  SIMTRP.trp_status_conf_proc_sup%TYPE;
              OLD_obs_conf_proc_sup     SIMTRP.trp_obs_conf_proc_sup%TYPE;
              OLD_excl_policial         SIMTRP.trp_excl_policial%TYPE;
              OLD_dat_ini_contagem      SIMTRP.trp_dat_ini_contagem%TYPE;
              OLD_dat_fim_contagem      SIMTRP.trp_dat_fim_contagem%TYPE;
              OLD_opcao_ben_pm          SIMTRP.trp_opcao_ben_pm%TYPE;
              OLD_sub_opcao_ben_pm      SIMTRP.trp_sub_opcao_ben_pm%TYPE;
              OLD_cargo_promocao        SIMTRP.trp_cargo_promocao%TYPE;
              OLD_num_certidao_rats     SIMTRP.trp_num_certidao_rats%TYPE;
              OLD_dia_con_03_06         SIMTRP.trp_dia_con_03_06%TYPE;
              OLD_num_laudo_medico      SIMTRP.trp_num_laudo_medico%TYPE;
              OLD_num_ord_jud           SIMTRP.trp_num_ord_jud%TYPE;
              OLD_opcao_com_sem_media   SIMTRP.trp_opcao_com_sem_media%TYPE;
              OLD_dia_con_03_04         SIMTRP.trp_dia_con_03_04%TYPE;
              OLD_dia_con_04_05         SIMTRP.trp_dia_con_04_05%TYPE;
              OLD_ano_ser_crg_efe_03    SIMTRP.trp_ano_ser_crg_efe_03%TYPE;
              OLD_flg_tempo_crg_efe_act SIMTRP.trp_flg_tempo_crg_efe_act%TYPE;
              OLD_dat_mes_ficha         SIMTRP.trp_dat_mes_ficha%TYPE;
              OLD_dat_ano_ficha               SIMTRP.trp_dat_ano_ficha%TYPE;
              OLD_ano_ser_crg_efe_prof        SIMTRP.trp_ano_ser_crg_efe_prof%TYPE;
              OLD_anos_trabalhados_prof       SIMTRP.trp_anos_trabalhados_prof%TYPE;
              OLD_dias_trabalhados_prof       SIMTRP.trp_dias_trabalhados_prof%TYPE;
              OLD_dias_trab_antes_98_prof     SIMTRP.trp_dias_trab_antes_98_prof%TYPE;
              OLD_anos_trab_antes_98_prof     SIMTRP.trp_anos_trab_antes_98_prof%TYPE;
              OLD_anos_trab_depois_03_prof    SIMTRP.trp_anos_trab_depois_03_prof%TYPE;
              OLD_dias_trab_depois_03_prof    SIMTRP.trp_dias_trab_depois_03_prof%TYPE;
              OLD_dias_trab_98_03_prof        SIMTRP.trp_dias_trab_98_03_prof%TYPE;
              OLD_anos_trab_98_03_prof        SIMTRP.trp_anos_trab_98_03_prof%TYPE;
              OLD_dias_trab_03_04_prof        SIMTRP.trp_dias_trab_03_04_prof%TYPE;
              OLD_dias_trab_04_05_prof        SIMTRP.trp_dias_trab_04_05_prof%TYPE;
              OLD_dia_averbado_prof           SIMTRP.trp_dia_averbado_prof%TYPE;
              OLD_dias_trab_03_06_prof        SIMTRP.trp_dias_trab_03_06_prof%TYPE;
              OLD_anos_trab_03_06_prof        SIMTRP.trp_anos_trab_03_06_prof%TYPE;
              OLD_qtd_faltas_desc_apo_prof    SIMTRP.trp_qtd_faltas_desc_apo_prof%TYPE;
              OLD_dias_desc_prof_cessao       SIMTRP.trp_dias_desc_prof_cessao%TYPE;
              OLD_dias_desc_prof_designacao   SIMTRP.trp_dias_desc_prof_designacao%TYPE;
              OLD_dia_ser_pub_prof            SIMTRP.trp_dia_ser_pub_prof%TYPE;
              OLD_ano_ser_crg_efe_03_prof     SIMTRP.trp_ano_ser_crg_efe_03_prof%TYPE;
              OLD_dia_carreira_prof           SIMTRP.trp_dia_carreira_prof%TYPE;
              OLD_dat_agregacao               SIMTRP.trp_dat_agregacao%TYPE;
              OLD_flg_agreg_julg_invalido     SIMTRP.trp_flg_agreg_julg_invalido%TYPE;
              OLD_cod_tipo_ben_apo_val_est    SIMTRP.trp_cod_tipo_ben_apo_val_est%TYPE;
              OLD_data_volunt_apo_val_est     SIMTRP.trp_data_volunt_apo_val_est%TYPE;
              OLD_flg_gera_resumo_despacho    SIMTRP.trp_flg_gera_resumo_despacho%TYPE;
              OLD_qtd_iamspe_agregado         SIMTRP.trp_qtd_iamspe_agregado%TYPE;
              OLD_flg_utiliza_retroativo      SIMTRP.trp_flg_utiliza_retroativo%TYPE;
              OLD_data_ingresso_carreira      SIMTRP.trp_data_ingresso_carreira%TYPE;
              OLD_data_ingresso_cargo         SIMTRP.trp_data_ingresso_cargo%TYPE;
              OLD_dias_ser_crg_efe            SIMTRP.trp_dias_ser_crg_efe%TYPE;
              OLD_dias_ser_crg_efe_03         SIMTRP.trp_dias_ser_crg_efe_03%TYPE;
              OLD_inatividade_art52           SIMTRP.trp_inatividade_art52%TYPE;
              OLD_tot_dia_con_art52           SIMTRP.trp_tot_dia_con_art52%TYPE;
              OLD_cod_seq_inat_nao_elegivel   SIMTRP.trp_cod_seq_inat_nao_elegivel%TYPE;
              OLD_dat_retroativo              SIMTRP.trp_dat_retroativo%TYPE;
              OLD_cod_quadro_atv              SIMTRP.trp_cod_quadro_atv%TYPE;
              OLD_tot_dia_con_edit            SIMTRP.trp_tot_dia_con_edit%TYPE;
              OLD_tot_dia_averbado_edit       SIMTRP.trp_tot_dia_averbado_edit%TYPE;
              OLD_flg_cruz_azul               SIMTRP.trp_flg_cruz_azul%TYPE;
              OLD_num_bol_insal               SIMTRP.trp_num_bol_insal%TYPE;
              OLD_qtd_quinquenios             SIMTRP.trp_qtd_quinquenios%TYPE;
              OLD_pub_98                      SIMTRP.trp_pub_98%TYPE;
              OLD_pub                         SIMTRP.trp_pub%TYPE;
              OLD_anos_que_se_aposenta        SIMTRP.trp_anos_que_se_aposenta%TYPE;
              OLD_dias_que_se_aposenta        SIMTRP.trp_dias_que_se_aposenta%TYPE;
              OLD_anos_que_se_aposenta_03     SIMTRP.trp_anos_que_se_aposenta_03%TYPE;
              OLD_dias_que_se_aposenta_03     SIMTRP.trp_dias_que_se_aposenta_03%TYPE;
              OLD_adicionais_nao_pub_inat     SIMTRP.trp_adicionais_nao_pub_inat%TYPE;
              OLD_dat_ult_prom_inat           SIMTRP.trp_dat_ult_prom_inat%TYPE;
              OLD_pub_prom_inat               SIMTRP.trp_pub_prom_inat%TYPE;
              OLD_flg_utiliza_iamspe          SIMTRP.trp_flg_utiliza_iamspe%TYPE;
              OLD_dias_desc_comis_crg_efe     SIMTRP.trp_dias_desc_comis_crg_efe%TYPE;
              OLD_dias_desc_comis_crg_efe_03  SIMTRP.trp_dias_desc_comis_crg_efe_03%TYPE;
              OLD_dat_inicio_serv_publico     SIMTRP.trp_dat_inicio_serv_publico%TYPE;
              OLD_doe_rad                     SIMTRP.trp_doe_rad%TYPE;
              OLD_dat_doe                     SIMTRP.trp_dat_doe%TYPE;
              OLD_dia_averbado_rgps           SIMTRP.trp_dia_averbado_rgps%TYPE;
              OLD_dia_averbado_rgps_prof      SIMTRP.trp_dia_averbado_rgps_prof%TYPE;
              OLD_dia_averbado_rgps_calend    SIMTRP.trp_dia_averbado_rgps_calend%TYPE;
              OLD_dia_averbado_rgps_cal_prof  SIMTRP.trp_dia_averbado_rgps_cal_prof%TYPE;
              OLD_pub_03                      SIMTRP.trp_pub_03%TYPE;
              OLD_dia_ser_pub_03              SIMTRP.trp_dia_ser_pub_03%TYPE;
              OLD_dia_ser_pub_03_prof         SIMTRP.trp_dia_ser_pub_03_prof%TYPE;
              OLD_dat_trib_just_mil_inat      SIMTRP.trp_dat_trib_just_mil_inat%TYPE;
              OLD_qtd_afast_desc_apo          SIMTRP.trp_qtd_afast_desc_apo%TYPE;
              OLD_qtd_penal_desc_apo          SIMTRP.trp_qtd_penal_desc_apo%TYPE;
              OLD_qtd_afast_desc_apo_prof     SIMTRP.trp_qtd_afast_desc_apo_prof%TYPE;
              OLD_qtd_penal_desc_apo_prof     SIMTRP.trp_qtd_penal_desc_apo_prof%TYPE;
              OLD_tip_aposentadoria_especial  SIMTRP.trp_tip_aposentadoria_especial%TYPE;
              OLD_dia_trab_ant_98_esp         SIMTRP.trp_dia_trab_ant_98_esp%TYPE;
              OLD_dia_trab_de_98_ate_03_esp   SIMTRP.trp_dia_trab_de_98_ate_03_esp%TYPE;
              OLD_dia_trab_dep_03_esp         SIMTRP.trp_dia_trab_dep_03_esp%TYPE;
              OLD_dia_trab_03_ate_06_esp      SIMTRP.trp_dia_trab_03_ate_06_esp%TYPE;
              OLD_dia_trab_03_ate_04_esp      SIMTRP.trp_dia_trab_03_ate_04_esp%TYPE;
              OLD_dia_trab_04_ate_05_esp      SIMTRP.trp_dia_trab_04_ate_05_esp%TYPE;
              OLD_dia_carreira_ate_03         SIMTRP.trp_dia_carreira_ate_03%TYPE;
              OLD_dia_nivel                   SIMTRP.trp_dia_nivel%TYPE;
              OLD_flg_int_evol_1dia_mais      SIMTRP.trp_flg_int_evol_1dia_mais%TYPE;
              OLD_dat_ficha_financeira        SIMTRP.trp_dat_ficha_financeira%TYPE;
              OLD_por_pen_pro_orig            SIMTRP.trp_por_pen_pro_orig%TYPE;
              OLD_tot_dia_con_apos_recalculo  SIMTRP.trp_tot_dia_con_apos_recalculo%TYPE;
              OLD_dia_averbado_esp            SIMTRP.trp_dia_averbado_esp%TYPE;
              OLD_dat_primeira_evol           SIMTRP.trp_dat_primeira_evol%TYPE;
              OLD_flg_exerce_cmdo             SIMTRP.trp_flg_exerce_cmdo%TYPE;
              OLD_dat_fim_exerc_cmdo          SIMTRP.trp_dat_fim_exerc_cmdo%TYPE;
              OLD_dia_agente_insalubre        SIMTRP.trp_dia_agente_insalubre%TYPE;
              OLD_dia_agente_insalubre        SIMTRP.  trp_dia_agente_insalubre%TYPE;

    BEGIN

       
       BEGIN
        SELECT
              trp_dia_con_reg_ant,
              trp_dia_con_reg_nue,
              trp_tot_dia_con,
              trp_dia_exi_pen_int,
              trp_dia_fal_pen_int,
              trp_ind_inv_may,
              trp_fec_dic_inv,
              trp_fec_ini_incap,
              trp_fec_est_parto,
              trp_ind_exi_abo,
              trp_ind_fal_ina,
              trp_fec_fal,
              trp_fec_reclu,
              trp_ley_reg_ant,
              trp_reg_jur_ant,
              trp_lic_pre_dia,
              trp_ind_pro_dis,
              trp_ind_deb_era,
              trp_tie_ser_pol,
              trp_dia_mag_ant,
              trp_dia_mag_nue,
              trp_fec_sol_pub,
              trp_fec_ini_pub,
              trp_fec_fin_pub,
              trp_fec_exp,
              trp_cod_est,
              trp_cod_sec,
              trp_entrada_tramite,
              trp_dia_con_98_03,
              trp_dia_carreira,
              trp_cod_ini_tra,
              trp_invalidez_integral,
              trp_dia_averbado,
              trp_dat_ped_20,
              trp_dat_ped_40,
              trp_ind_sim_ben,
              trp_tip_tra_fim,
              trp_cod_ben,
              trp_ind_par,
              trp_status,
              trp_obs,
              trp_tip_contrib,
              trp_cod_mot_deslig,
              trp_ind_prefeito,
              trp_ide_rel_func,
              trp_cod_tra_compl,
              trp_cod_entidade,
              trp_ind_acid_tra,
              trp_excl_magist_docente,
              trp_qtd_faltas_desc_apo,
              trp_status_proc_tec,
              trp_obs_proc_tec,
              trp_status_conf_proc_sup,
              trp_obs_conf_proc_sup,
              trp_excl_policial,
              trp_dat_ini_contagem,
              trp_dat_fim_contagem,
              trp_opcao_ben_pm,
              trp_sub_opcao_ben_pm,
              trp_cargo_promocao,
              trp_num_certidao_rats,
              trp_dia_con_03_06,
              trp_num_laudo_medico,
              trp_num_ord_jud,
              trp_opcao_com_sem_media,
              trp_dia_con_03_04,
              trp_dia_con_04_05,
              trp_ano_ser_crg_efe_03,
              trp_flg_tempo_crg_efe_act,
              trp_dat_mes_ficha,
              trp_dat_ano_ficha,
              trp_ano_ser_crg_efe_prof,
              trp_anos_trabalhados_prof,
              trp_dias_trabalhados_prof,
              trp_dias_trab_antes_98_prof,
              trp_anos_trab_antes_98_prof,
              trp_anos_trab_depois_03_prof,
              trp_dias_trab_depois_03_prof,
              trp_dias_trab_98_03_prof,
              trp_anos_trab_98_03_prof,
              trp_dias_trab_03_04_prof,
              trp_dias_trab_04_05_prof,
              trp_dia_averbado_prof,
              trp_dias_trab_03_06_prof,
              trp_anos_trab_03_06_prof,
              trp_qtd_faltas_desc_apo_prof,
              trp_dias_desc_prof_cessao,
              trp_dias_desc_prof_designacao,
              trp_dia_ser_pub_prof,
              trp_ano_ser_crg_efe_03_prof,
              trp_dia_carreira_prof,
              trp_dat_agregacao,
              trp_flg_agreg_julg_invalido,
              trp_cod_tipo_ben_apo_val_est,
              trp_data_volunt_apo_val_est,
              trp_flg_gera_resumo_despacho,
              trp_qtd_iamspe_agregado,
              trp_flg_utiliza_retroativo,
              trp_data_ingresso_carreira,
              trp_data_ingresso_cargo,
              trp_dias_ser_crg_efe,
              trp_dias_ser_crg_efe_03,
              trp_inatividade_art52,
              trp_tot_dia_con_art52,
              trp_cod_seq_inat_nao_elegivel,
              trp_dat_retroativo,
              trp_cod_quadro_atv,
              trp_tot_dia_con_edit,
              trp_tot_dia_averbado_edit,
              trp_flg_cruz_azul,
              trp_num_bol_insal,
              trp_qtd_quinquenios,
              trp_pub_98,
              trp_pub,
              trp_anos_que_se_aposenta,
              trp_dias_que_se_aposenta,
              trp_anos_que_se_aposenta_03,
              trp_dias_que_se_aposenta_03,
              trp_adicionais_nao_pub_inat,
              trp_dat_ult_prom_inat,
              trp_pub_prom_inat,
              trp_flg_utiliza_iamspe,
              trp_dias_desc_comis_crg_efe,
              trp_dias_desc_comis_crg_efe_03,
              trp_dat_inicio_serv_publico,
              trp_doe_rad,
              trp_dat_doe,
              trp_dia_averbado_rgps,
              trp_dia_averbado_rgps_prof,
              trp_dia_averbado_rgps_calend,
              trp_dia_averbado_rgps_cal_prof,
              trp_pub_03,
              trp_dia_ser_pub_03,
              trp_dia_ser_pub_03_prof,
              trp_dat_trib_just_mil_inat,
              trp_qtd_afast_desc_apo,
              trp_qtd_penal_desc_apo,
              trp_qtd_afast_desc_apo_prof,
              trp_qtd_penal_desc_apo_prof,
              trp_tip_aposentadoria_especial,
              trp_dia_trab_ant_98_esp,
              trp_dia_trab_de_98_ate_03_esp,
              trp_dia_trab_dep_03_esp,
              trp_dia_trab_03_ate_06_esp,
              trp_dia_trab_03_ate_04_esp,
              trp_dia_trab_04_ate_05_esp,
              trp_dia_carreira_ate_03,
              trp_dia_nivel,
              trp_flg_int_evol_1dia_mais,
              trp_dat_ficha_financeira,
              trp_por_pen_pro_orig,
              trp_tot_dia_con_apos_recalculo,
              trp_dia_averbado_esp,
              trp_dat_primeira_evol,
              trp_flg_exerce_cmdo,
              trp_dat_fim_exerc_cmdo

          INTO
                OLD_dia_con_reg_ant ,
                OLD_dia_con_reg_nue ,
                OLD_tot_dia_con ,
                OLD_dia_exi_pen_int ,
                OLD_dia_fal_pen_int ,
                OLD_ind_inv_may ,
                OLD_fec_dic_inv ,
                OLD_fec_ini_incap ,
                OLD_fec_est_parto ,
                OLD_ind_exi_abo ,
                OLD_ind_fal_ina ,
                OLD_fec_fal ,
                OLD_fec_reclu ,
                OLD_ley_reg_ant ,
                OLD_reg_jur_ant ,
                OLD_lic_pre_dia ,
                OLD_ind_pro_dis ,
                OLD_ind_deb_era ,
                OLD_tie_ser_pol ,
                OLD_dia_mag_ant ,
                OLD_dia_mag_nue ,
                OLD_fec_sol_pub ,
                OLD_fec_ini_pub ,
                OLD_fec_fin_pub ,
                OLD_fec_exp ,
                OLD_cod_est ,
                OLD_cod_sec ,
                OLD_entrada_tramite ,
                OLD_dia_con_98_03 ,
                OLD_dia_carreira  ,
                OLD_cod_ini_tra ,
                OLD_invalidez_integral  ,
                OLD_dia_averbado  ,
                OLD_dat_ped_20  ,
                OLD_dat_ped_40  ,
                OLD_ind_sim_ben ,
                OLD_tip_tra_fim ,
                OLD_cod_ben ,
                OLD_ind_par ,
                OLD_status  ,
                OLD_obs ,
                OLD_tip_contrib ,
                OLD_cod_mot_deslig  ,
                OLD_ind_prefeito  ,
                OLD_ide_rel_func  ,
                OLD_cod_tra_compl ,
                OLD_cod_entidade  ,
                OLD_ind_acid_tra  ,
                OLD_excl_magist_docente ,
                OLD_qtd_faltas_desc_apo ,
                OLD_status_proc_tec ,
                OLD_obs_proc_tec  ,
                OLD_status_conf_proc_sup  ,
                OLD_obs_conf_proc_sup ,
                OLD_excl_policial ,
                OLD_dat_ini_contagem  ,
                OLD_dat_fim_contagem  ,
                OLD_opcao_ben_pm  ,
                OLD_sub_opcao_ben_pm  ,
                OLD_cargo_promocao  ,
                OLD_num_certidao_rats ,
                OLD_dia_con_03_06 ,
                OLD_num_laudo_medico  ,
                OLD_num_ord_jud ,
                OLD_opcao_com_sem_media ,
                OLD_dia_con_03_04 ,
                OLD_dia_con_04_05 ,
                OLD_ano_ser_crg_efe_03  ,
                OLD_flg_tempo_crg_efe_act ,
                OLD_dat_mes_ficha ,
                OLD_dat_ano_ficha ,
                OLD_ano_ser_crg_efe_prof  ,
                OLD_anos_trabalhados_prof ,
                OLD_dias_trabalhados_prof ,
                OLD_dias_trab_antes_98_prof ,
                OLD_anos_trab_antes_98_prof ,
                OLD_anos_trab_depois_03_prof  ,
                OLD_dias_trab_depois_03_prof  ,
                OLD_dias_trab_98_03_prof  ,
                OLD_anos_trab_98_03_prof  ,
                OLD_dias_trab_03_04_prof  ,
                OLD_dias_trab_04_05_prof  ,
                OLD_dia_averbado_prof ,
                OLD_dias_trab_03_06_prof  ,
                OLD_anos_trab_03_06_prof  ,
                OLD_qtd_faltas_desc_apo_prof  ,
                OLD_dias_desc_prof_cessao ,
                OLD_dias_desc_prof_designacao ,
                OLD_dia_ser_pub_prof  ,
                OLD_ano_ser_crg_efe_03_prof ,
                OLD_dia_carreira_prof ,
                OLD_dat_agregacao ,
                OLD_flg_agreg_julg_invalido ,
                OLD_cod_tipo_ben_apo_val_est  ,
                OLD_data_volunt_apo_val_est ,
                OLD_flg_gera_resumo_despacho  ,
                OLD_qtd_iamspe_agregado ,
                OLD_flg_utiliza_retroativo  ,
                OLD_data_ingresso_carreira  ,
                OLD_data_ingresso_cargo ,
                OLD_dias_ser_crg_efe  ,
                OLD_dias_ser_crg_efe_03 ,
                OLD_inatividade_art52 ,
                OLD_tot_dia_con_art52 ,
                OLD_cod_seq_inat_nao_elegivel ,
                OLD_dat_retroativo  ,
                OLD_cod_quadro_atv  ,
                OLD_tot_dia_con_edit  ,
                OLD_tot_dia_averbado_edit ,
                OLD_flg_cruz_azul ,
                OLD_num_bol_insal ,
                OLD_qtd_quinquenios ,
                OLD_pub_98  ,
                OLD_pub ,
                OLD_anos_que_se_aposenta  ,
                OLD_dias_que_se_aposenta  ,
                OLD_anos_que_se_aposenta_03 ,
                OLD_dias_que_se_aposenta_03 ,
                OLD_adicionais_nao_pub_inat ,
                OLD_dat_ult_prom_inat ,
                OLD_pub_prom_inat ,
                OLD_flg_utiliza_iamspe  ,
                OLD_dias_desc_comis_crg_efe ,
                OLD_dias_desc_comis_crg_efe_03  ,
                OLD_dat_inicio_serv_publico ,
                OLD_doe_rad ,
                OLD_dat_doe ,
                OLD_dia_averbado_rgps ,
                OLD_dia_averbado_rgps_prof  ,
                OLD_dia_averbado_rgps_calend  ,
                OLD_dia_averbado_rgps_cal_prof  ,
                OLD_pub_03  ,
                OLD_dia_ser_pub_03  ,
                OLD_dia_ser_pub_03_prof ,
                OLD_dat_trib_just_mil_inat  ,
                OLD_qtd_afast_desc_apo  ,
                OLD_qtd_penal_desc_apo  ,
                OLD_qtd_afast_desc_apo_prof ,
                OLD_qtd_penal_desc_apo_prof ,
                OLD_tip_aposentadoria_especial  ,
                OLD_dia_trab_ant_98_esp ,
                OLD_dia_trab_de_98_ate_03_esp ,
                OLD_dia_trab_dep_03_esp ,
                OLD_dia_trab_03_ate_06_esp  ,
                OLD_dia_trab_03_ate_04_esp  ,
                OLD_dia_trab_04_ate_05_esp  ,
                OLD_dia_carreira_ate_03 ,
                OLD_dia_nivel ,
                OLD_flg_int_evol_1dia_mais  ,
                OLD_dat_ficha_financeira  ,
                OLD_por_pen_pro_orig  ,
                OLD_tot_dia_con_apos_recalculo  ,
                OLD_dia_averbado_esp  ,
                OLD_dat_primeira_evol ,
                OLD_flg_exerce_cmdo ,
                OLD_dat_fim_exerc_cmdo


          FROM SIMTRP SP
          WHERE
             EXISTS (
             SELECT 1 FROM TB_CONCESSAO_BENEFICIO CC, SIMTRP NSP
             WHERE NSP.TRP_COD_INS        = PAR_COD_INS          AND
                   NSP.TRP_COD_ADM_TRA    = IN_COD_ADM_TRA       AND
                   CC.COD_INS             = NSP.TRP_COD_INS      AND
                   CC.COD_ENTIDADE        = NSP.TRP_COD_ENTIDADE AND
                   CC.NUM_MATRICULA       = NSP.TRP_NUM_MAT      AND
                   CC.COD_IDE_REL_FUNC    = NSP.TRP_IDE_REL_FUNC AND
                   CC.COD_IDE_CLI_SERV    = NSP.TRP_IDE_CLI      AND
                   CC.COD_TIPO_BENEFICIO !='M'                   AND
                   SP.TRP_COD_INS        = NSP.TRP_COD_INS       AND
                   SP.TRP_COD_ENTIDADE   = NSP.TRP_COD_ENTIDADE  AND
                   SP.TRP_NUM_MAT        = NSP.TRP_NUM_MAT       AND
                   SP.TRP_IDE_REL_FUNC   = NSP.TRP_IDE_REL_FUNC  AND
                   SP.TRP_IDE_CLI        = NSP.TRP_IDE_CLI       AND
                   SP.TRP_IND_TIP_TRA    NOT IN ('M','MM')


             );

           EXCEPTION
                 When OTHERS Then
                     
                     RETURN -1;
           END;

     BEGIN
       UPDATE SIMTRP SPTRP SET

              trp_dia_con_reg_ant    =    OLD_dia_con_reg_ant ,
              trp_dia_con_reg_nue    =    OLD_dia_con_reg_nue ,
              trp_tot_dia_con        =    OLD_tot_dia_con ,
              trp_dia_exi_pen_int    =    OLD_dia_exi_pen_int ,
              trp_dia_fal_pen_int    =    OLD_dia_fal_pen_int ,
              trp_ind_inv_may        =    OLD_ind_inv_may ,
              trp_fec_dic_inv        =    OLD_fec_dic_inv ,
              trp_fec_ini_incap      =    OLD_fec_ini_incap ,
              trp_fec_est_parto      =    OLD_fec_est_parto ,
              trp_ind_exi_abo        =    OLD_ind_exi_abo ,
              trp_ind_fal_ina        =    OLD_ind_fal_ina ,
              trp_fec_fal            =    OLD_fec_fal ,
              trp_fec_reclu          =    OLD_fec_reclu ,
              trp_ley_reg_ant        =    OLD_ley_reg_ant ,
              trp_reg_jur_ant        =    OLD_reg_jur_ant ,
              trp_lic_pre_dia        =    OLD_lic_pre_dia ,
              trp_ind_pro_dis        =    OLD_ind_pro_dis ,
              trp_ind_deb_era        =    OLD_ind_deb_era ,
              trp_tie_ser_pol        =    OLD_tie_ser_pol ,
              trp_dia_mag_ant        =    OLD_dia_mag_ant ,
              trp_dia_mag_nue        =    OLD_dia_mag_nue ,
              trp_fec_sol_pub        =    OLD_fec_sol_pub ,
              trp_fec_ini_pub        =     OLD_fec_ini_pub ,
              trp_fec_fin_pub        =    OLD_fec_fin_pub ,
              trp_fec_exp            =    OLD_fec_exp ,
              trp_cod_est            =    OLD_cod_est ,
              trp_cod_sec            =    OLD_cod_sec ,
              trp_entrada_tramite    =    OLD_entrada_tramite ,
              trp_dia_con_98_03      =    OLD_dia_con_98_03 ,
              trp_dia_carreira       =    OLD_dia_carreira  ,
              trp_cod_ini_tra        =    OLD_cod_ini_tra ,
              trp_invalidez_integral =    OLD_invalidez_integral  ,
              trp_dia_averbado       =    OLD_dia_averbado  ,
              trp_dat_ped_20         =    OLD_dat_ped_20  ,
              trp_dat_ped_40         =    OLD_dat_ped_40  ,
              trp_ind_sim_ben        =    OLD_ind_sim_ben ,
              trp_tip_tra_fim        =    OLD_tip_tra_fim ,
              trp_cod_ben            =    OLD_cod_ben ,
              trp_ind_par            =    OLD_ind_par ,
              trp_status             =    OLD_status  ,
              trp_obs                =    OLD_obs ,
              trp_tip_contrib        =    OLD_tip_contrib ,
              trp_cod_mot_deslig     =    OLD_cod_mot_deslig  ,
              trp_ind_prefeito       =    OLD_ind_prefeito  ,
              trp_ide_rel_func       =    OLD_ide_rel_func  ,
              trp_cod_tra_compl      =    OLD_cod_tra_compl ,
              trp_cod_entidade       =    OLD_cod_entidade  ,
              trp_ind_acid_tra       =    OLD_ind_acid_tra  ,
              trp_excl_magist_docente=    OLD_excl_magist_docente ,
              trp_qtd_faltas_desc_apo  =    OLD_qtd_faltas_desc_apo ,
              trp_status_proc_tec       =   OLD_status_proc_tec ,
              trp_obs_proc_tec          =   OLD_obs_proc_tec  ,
              trp_status_conf_proc_sup  =   OLD_status_conf_proc_sup  ,
              trp_obs_conf_proc_sup     =   OLD_obs_conf_proc_sup ,
              trp_excl_policial         =   OLD_excl_policial ,
              trp_dat_ini_contagem      =   OLD_dat_ini_contagem  ,
              trp_dat_fim_contagem      =   OLD_dat_fim_contagem  ,
              trp_opcao_ben_pm          =   OLD_opcao_ben_pm  ,
              trp_sub_opcao_ben_pm      =   OLD_sub_opcao_ben_pm  ,
              trp_cargo_promocao        =   OLD_cargo_promocao  ,
              trp_num_certidao_rats     =   OLD_num_certidao_rats ,
              trp_dia_con_03_06         =   OLD_dia_con_03_06 ,
              trp_num_laudo_medico      =   OLD_num_laudo_medico  ,
              trp_num_ord_jud           =   OLD_num_ord_jud ,
              trp_opcao_com_sem_media   =   OLD_opcao_com_sem_media ,
              trp_dia_con_03_04         =   OLD_dia_con_03_04 ,
              trp_dia_con_04_05         =   OLD_dia_con_04_05 ,
              trp_ano_ser_crg_efe_03    =   OLD_ano_ser_crg_efe_03  ,
              trp_flg_tempo_crg_efe_act    =   OLD_flg_tempo_crg_efe_act ,
              trp_dat_mes_ficha            =   OLD_dat_mes_ficha ,
              trp_dat_ano_ficha            =   OLD_dat_ano_ficha ,
              trp_ano_ser_crg_efe_prof     =   OLD_ano_ser_crg_efe_prof  ,
              trp_anos_trabalhados_prof    =   OLD_anos_trabalhados_prof ,
              trp_dias_trabalhados_prof    =   OLD_dias_trabalhados_prof ,
              trp_dias_trab_antes_98_prof  =   OLD_dias_trab_antes_98_prof ,
              trp_anos_trab_antes_98_prof  =   OLD_anos_trab_antes_98_prof ,
              trp_anos_trab_depois_03_prof =   OLD_anos_trab_depois_03_prof  ,
              trp_dias_trab_depois_03_prof =   OLD_dias_trab_depois_03_prof  ,
              trp_dias_trab_98_03_prof     =   OLD_dias_trab_98_03_prof  ,
              trp_anos_trab_98_03_prof     =   OLD_anos_trab_98_03_prof  ,
              trp_dias_trab_03_04_prof     =   OLD_dias_trab_03_04_prof  ,
              trp_dias_trab_04_05_prof     =   OLD_dias_trab_04_05_prof  ,
              trp_dia_averbado_prof        =   OLD_dia_averbado_prof ,
              trp_dias_trab_03_06_prof     =   OLD_dias_trab_03_06_prof  ,
              trp_anos_trab_03_06_prof     =   OLD_anos_trab_03_06_prof  ,
              trp_qtd_faltas_desc_apo_prof =   OLD_qtd_faltas_desc_apo_prof  ,
              trp_dias_desc_prof_cessao    =   OLD_dias_desc_prof_cessao ,
              trp_dias_desc_prof_designacao=   OLD_dias_desc_prof_designacao ,
              trp_dia_ser_pub_prof         =   OLD_dia_ser_pub_prof  ,
              trp_ano_ser_crg_efe_03_prof  =   OLD_ano_ser_crg_efe_03_prof ,
              trp_dia_carreira_prof        =   OLD_dia_carreira_prof ,
              trp_dat_agregacao            =   OLD_dat_agregacao ,
              trp_flg_agreg_julg_invalido  =   OLD_flg_agreg_julg_invalido ,
              trp_cod_tipo_ben_apo_val_est =   OLD_cod_tipo_ben_apo_val_est  ,
              trp_data_volunt_apo_val_est   =   OLD_data_volunt_apo_val_est ,
              trp_flg_gera_resumo_despacho  =   OLD_flg_gera_resumo_despacho  ,
              trp_qtd_iamspe_agregado       =   OLD_qtd_iamspe_agregado ,
              trp_flg_utiliza_retroativo    =   OLD_flg_utiliza_retroativo  ,
              trp_data_ingresso_carreira    =   OLD_data_ingresso_carreira  ,
              trp_data_ingresso_cargo       =   OLD_data_ingresso_cargo ,
              trp_dias_ser_crg_efe          =   OLD_dias_ser_crg_efe  ,
              trp_dias_ser_crg_efe_03       =   OLD_dias_ser_crg_efe_03 ,
              trp_inatividade_art52         =   OLD_inatividade_art52 ,
              trp_tot_dia_con_art52         =   OLD_tot_dia_con_art52 ,
              trp_cod_seq_inat_nao_elegivel =   OLD_cod_seq_inat_nao_elegivel ,
              trp_dat_retroativo            =   OLD_dat_retroativo  ,
              trp_cod_quadro_atv            =   OLD_cod_quadro_atv  ,
              trp_tot_dia_con_edit          =   OLD_tot_dia_con_edit  ,
              trp_tot_dia_averbado_edit     =   OLD_tot_dia_averbado_edit ,
              trp_flg_cruz_azul             =   OLD_flg_cruz_azul ,
              trp_num_bol_insal             =   OLD_num_bol_insal ,
              trp_qtd_quinquenios           =   OLD_qtd_quinquenios ,
              trp_pub_98                    =   OLD_pub_98  ,
              trp_pub                       =   OLD_pub ,
              trp_anos_que_se_aposenta      =   OLD_anos_que_se_aposenta  ,
              trp_dias_que_se_aposenta      =   OLD_dias_que_se_aposenta  ,
              trp_anos_que_se_aposenta_03   =   OLD_anos_que_se_aposenta_03 ,
              trp_dias_que_se_aposenta_03   =   OLD_dias_que_se_aposenta_03 ,
              trp_adicionais_nao_pub_inat   =   OLD_adicionais_nao_pub_inat ,
              trp_dat_ult_prom_inat         =   OLD_dat_ult_prom_inat ,
              trp_pub_prom_inat             =   OLD_pub_prom_inat ,
              trp_flg_utiliza_iamspe        =   OLD_flg_utiliza_iamspe  ,
              trp_dias_desc_comis_crg_efe   =   OLD_dias_desc_comis_crg_efe ,
              trp_dias_desc_comis_crg_efe_03=  OLD_dias_desc_comis_crg_efe_03  ,
              trp_dat_inicio_serv_publico   =  OLD_dat_inicio_serv_publico ,
              trp_doe_rad                   =  OLD_doe_rad ,
              trp_dat_doe                   =  OLD_dat_doe ,
              trp_dia_averbado_rgps         =  OLD_dia_averbado_rgps ,
              trp_dia_averbado_rgps_prof    =  OLD_dia_averbado_rgps_prof  ,
              trp_dia_averbado_rgps_calend  =  OLD_dia_averbado_rgps_calend  ,
              trp_dia_averbado_rgps_cal_prof=  OLD_dia_averbado_rgps_cal_prof  ,
              trp_pub_03                    =  OLD_pub_03  ,
              trp_dia_ser_pub_03            =  OLD_dia_ser_pub_03  ,
              trp_dia_ser_pub_03_prof       =  OLD_dia_ser_pub_03_prof ,
              trp_dat_trib_just_mil_inat    =  OLD_dat_trib_just_mil_inat  ,
              trp_qtd_afast_desc_apo        =  OLD_qtd_afast_desc_apo  ,
              trp_qtd_penal_desc_apo        =  OLD_qtd_penal_desc_apo  ,
              trp_qtd_afast_desc_apo_prof   =  OLD_qtd_afast_desc_apo_prof ,
              trp_qtd_penal_desc_apo_prof   =  OLD_qtd_penal_desc_apo_prof ,
              trp_tip_aposentadoria_especial=  OLD_tip_aposentadoria_especial  ,
              trp_dia_trab_ant_98_esp       =  OLD_dia_trab_ant_98_esp ,
              trp_dia_trab_de_98_ate_03_esp =  OLD_dia_trab_de_98_ate_03_esp ,
              trp_dia_trab_dep_03_esp       =  OLD_dia_trab_dep_03_esp ,
              trp_dia_trab_03_ate_06_esp    =  OLD_dia_trab_03_ate_06_esp  ,
              trp_dia_trab_03_ate_04_esp    =  OLD_dia_trab_03_ate_04_esp  ,
              trp_dia_trab_04_ate_05_esp    =  OLD_dia_trab_04_ate_05_esp  ,
              trp_dia_carreira_ate_03       =  OLD_dia_carreira_ate_03 ,
              trp_dia_nivel                 =  OLD_dia_nivel ,
              trp_flg_int_evol_1dia_mais    =  OLD_flg_int_evol_1dia_mais  ,
              trp_dat_ficha_financeira      =  OLD_dat_ficha_financeira  ,
              trp_por_pen_pro_orig          =  OLD_por_pen_pro_orig  ,
              trp_tot_dia_con_apos_recalculo=  OLD_tot_dia_con_apos_recalculo  ,
              trp_dia_averbado_esp          =  OLD_dia_averbado_esp  ,
              trp_dat_primeira_evol         =  OLD_dat_primeira_evol ,
              trp_flg_exerce_cmdo           =  OLD_flg_exerce_cmdo ,
              trp_dat_fim_exerc_cmdo        =   OLD_dat_fim_exerc_cmdo

              WHERE   SPTRP.TRP_COD_INS        = PAR_COD_INS            AND
                      SPTRP.TRP_COD_ADM_TRA    = IN_COD_ADM_TRA ;

            EXCEPTION
                 When OTHERS Then
                      
                     RETURN 0;
           END;
           COMMIT;
           RETURN 0;


    END;

  ----- Ontem Regras de Pensao e Tipos de Beneficiario -----
  ----------------------------------------------------------


 
Procedure SP_EMQUADRA_BENEFICIARIOS ( P_COD_INS        IN  NUMBER ,
                                      P_COD_ADM_TRA    IN  VARCHAR,
                                      P_SEQ_BEN        IN  NUMBER ,
                                      P_COD_REGISTRO   IN  NUMBER ,
                                      OERRORCODE       OUT NUMBER ,
                                      OERRORMESSAGE    OUT VARCHAR) AS


  DAT_TERM_BENEFICIO DATE;
  DATE_TERM_FUTURA   DATE;
  BEGIN
   
    BEGIN    
  
         FOR BEN_P  IN (
                 SELECT    BEN.COD_IDE_CLI_SERV                ,
                           BEN.COD_IDE_CLI_BEN                 ,
                           BEN.DAT_INI_BEN                     ,
                           BEN.DAT_INI_DEP                     ,
                           FSER.DAT_OBITO                      ,
                           SER.FLG_FALECEU_ATIVIDADE           ,
                           SER.FLG_OBITO_DECORRENTE_EXERC_RAZAO,
                           SER.FLG_FALECEU_ACID_DOENCA         ,
                           BEN.COD_PERFIL_BENEFICIARIO         ,
                           BEN.FLG_INVALIDO_DEFICIENCIA        ,
                           MONTHS_BETWEEN( FSER.DAT_OBITO, BEN.DAT_INI_BEN  ) QTD_ANOS_DEP,
                           MONTHS_BETWEEN( FSER.DAT_OBITO,FDEP.DAT_NASC ) IDADE_DEP,
                           SER.QTD_TMP_CONTR_DIAS_TOTAL/360     AS QTD_TMP_CONTR_DIAS_TOTAL 
                          
                    FROM TB_NPM_BENEFICIARIO BEN   ,
                         TB_NPM_SERVIDOR     SER   ,
                         TB_PESSOA_FISICA    FSER  ,
                         TB_PESSOA_FISICA    FDEP  
                   WHERE BEN.COD_INS       = P_COD_INS            AND 
                         BEN.COD_ADM_TRA   = P_COD_ADM_TRA        AND
                         BEN.COD_REGISTRO  = P_COD_REGISTRO       AND
                         FSER.COD_INS      = P_COD_INS            AND
                         FSER.COD_IDE_CLI  = BEN.COD_IDE_CLI_SERV AND
                         SER.COD_INS       = P_COD_INS            AND
                         SER.COD_ADM_TRA   = P_COD_ADM_TRA        AND
                         SER.COD_REGISTRO  = P_COD_REGISTRO       AND  
                         FDEP.COD_INS      = P_COD_INS            AND
                         FDEP.COD_IDE_CLI  = BEN.COD_IDE_CLI_BEN
         ) LOOP
             DAT_TERM_BENEFICIO:=NULL;


                    FOR REGRA_BENEF IN ( 
                              SELECT DISTINCT 
                                    A.SEQ_BEN, A.SEQ_REGRA_GRUPO_PAG,
                                    A.FLG_OBITO_FUNCAO_NPM      ,  A.FLG_OBITO_EXERC_POLICIA AS  FLG_OBITO_DECORRENTE_EXERC_RAZAO ,
                                    A.FLG_FALECEU_ATIVIDADE     ,  A.FLG_FALECEU_ACID_DOENCA          , 
                                    A.COD_PERFIL                , 
                                    FLG_INVALIDO_DEFICIENCIA    ,
                                    A.IDADE_MIN,A.IDADE_MAX     , 
                                    A.TEMPO_CONTR_MIN           ,
                                    A.TEMPO_MIN_DEPENDENCIA     ,
                                    A.QTD_ANOS_BENEFICIO
                         FROM TB_PERFIL_BENEFICIARO_NLP A 
                         WHERE 
                              A.COD_INS = P_COD_INS   AND
                              A.SEQ_BEN = P_SEQ_BEN  
               
                     ) LOOP
                       
                          IF   REGRA_BENEF.COD_PERFIL= BEN_P.COD_PERFIL_BENEFICIARIO  THEN  
                            
                                  IF NVL(REGRA_BENEF.FLG_OBITO_FUNCAO_NPM,'N')               = 
                                     NVL(BEN_P.FLG_FALECEU_ATIVIDADE,'N')               AND
                                     
                                     NVL(REGRA_BENEF.FLG_OBITO_DECORRENTE_EXERC_RAZAO,'N')   = 
                                     NVL(BEN_P.FLG_OBITO_DECORRENTE_EXERC_RAZAO,'N')    AND
                                     
                                     --NVL(REGRA_BENEF.FLG_FALECEU_ATIVIDADE,'N')              = 
                                     --NVL(BEN_P.FLG_FALECEU_ATIVIDADE,'N')                AND
                                     
                                     NVL(REGRA_BENEF.FLG_FALECEU_ACID_DOENCA,'N')            = 
                                     NVL(BEN_P.FLG_FALECEU_ACID_DOENCA,'N')              AND
                                     
                                     NVL(REGRA_BENEF.FLG_INVALIDO_DEFICIENCIA,'N')           =
                                     NVL(BEN_P.FLG_INVALIDO_DEFICIENCIA,'N')  
                                   THEN
                                     
                                      IF  NVL(REGRA_BENEF.IDADE_MIN,0)               <= BEN_P.IDADE_DEP                        AND
                                          NVL(REGRA_BENEF.IDADE_MAX,999)             >= BEN_P.IDADE_DEP                        AND
                                          NVL(REGRA_BENEF.TEMPO_CONTR_MIN,0)         <= NVL(BEN_P.QTD_TMP_CONTR_DIAS_TOTAL,0)  AND
                                          NVL(REGRA_BENEF.TEMPO_MIN_DEPENDENCIA,0)  <=  NVL(BEN_P.QTD_ANOS_DEP,0)
                                          
                                        THEN
                                           IF REGRA_BENEF.QTD_ANOS_BENEFICIO= 999 THEN 
                                               DAT_TERM_BENEFICIO:= NULL;
                                            ELSE
                                               DATE_TERM_FUTURA := ADD_MONTHS(BEN_P.DAT_OBITO,REGRA_BENEF.QTD_ANOS_BENEFICIO*24);
                                               IF DATE_TERM_FUTURA >= NVL(DAT_TERM_BENEFICIO,DATE_TERM_FUTURA ) THEN 
                                                  DAT_TERM_BENEFICIO:=  DATE_TERM_FUTURA;
                                               END IF;
                                            END IF;
                                        END IF;
                                   
                                   END IF;
                               
                             END IF;   
                          
                        END LOOP; 
                                ---- Atualiza % da aposentadoria legado
                                --- Executado em Java 
/*                                UPDATE TB_NPM_BENEFICIARIO NPM_BEN
                                       SET NPM_BEN.DAT_FIM_BEN_PREVISTA= DAT_TERM_BENEFICIO
                                       WHERE NPM_BEN.COD_INS      =   P_COD_INS        AND
                                             NPM_BEN.COD_ADM_TRA  =   P_COD_ADM_TRA    AND
                                             NPM_BEN.COD_REGISTRO =   PAR_COD_REGISTRO AND
                                             NPM_BEN.COD_IDE_CLI_SERV=BEN_P.COD_IDE_CLI_SERV AND
                                             NPM_BEN.COD_IDE_CLI_BEN =BEN_P.COD_IDE_CLI_BEN;*/
     
           END LOOP;
 
  
     EXCEPTION
        WHEN OTHERS THEN
                 OERRORCODE       :=30;
                 OERRORMESSAGE    := 'Erro ao emquadrar termino do beneficio';

  END;

 END;
  
  Procedure SP_CALCULA_REGRA_PENSAO (
                                     IN_TRP_SEP       IN NUMBER ,  
                                     IN_COD_ADM_TRA   IN VARCHAR, 
                                     IN_COD_REGISTRO  IN NUMBER,
                                     OERRORCODE       OUT NUMBER,
                                     OERRORMESSAGE    OUT VARCHAR 
   )
  IS
  
  V_CURSOR               CURSOR_BENCAL;--LINHA D0 CURSOR.
  V_LINHA                BENCAL%ROWTYPE;
  COUNTROWS              NUMBER;
  DESINFORMACAO          VARCHAR2(10);
  ADMTRAAUX              VARCHAR2(10);
  CODBENEFICIO           NUMBER;
  SEQBEN                 NUMBER(8);
  APROVADO               CHAR(1);
  PERCENT                NUMBER;
  PERCMIN                NUMBER;
  PERCMAX                NUMBER;
  CLASSE                 NUMBER;
  ADIC                   NUMBER;
  FLGCONTREXTRA          VARCHAR2(1);
  
   -------   Dados de Calculo  -----
  CGPA_flg_faleceu_atividade                    VARCHAR2(1);
  CGPA_flg_obito_decorrente_exerc_raza          VARCHAR2(1);
  CGPA_cod_beneficio_faleceu_atividade          NUMBER(8); 
  CGPA_flg_faleceu_acid_doenca                  VARCHAR2(1);
  CGPA_IND_TIP_CALC                             VARCHAR2(1);
  CGPA_POR_RETRIB                               NUMBER(8,4);
  CGPA_QTD_TMP_CONTR_DIAS_TOTAL                 NUMBER(8);
  CGPA_IND_TIP_COB                              VARCHAR2(1);      
  CGPA_POR_CALCULO                              NUMBER(8,4);
  CGPA_COD_TAREFA                               NUMBER(8);  
  CGPA_COD_REGISTRO                             NUMBER(8); 
  
BEGIN
      
  
      desInformacao        := '';
      codBeneficio         := 0;
      aprovado             := 'N';
      countrows            := 0;
      percent              := 100;
      percMin              := 0;
      percMax              := 0;
      admTraAux            := par_cod_adm_tra;
      oErrorCode           := 0;
 
     BEGIN



        GPAP_seq	            :=IN_TRP_SEP;
        GPAP_cod_ins	        :=PAR_COD_INS;
        GPAP_cod_ide_cli_serv	:=NULL ;
        GPAP_cod_adm_tra	    :=IN_COD_ADM_TRA;
        GPAP_FEC_PRO	        :=SYSDATE ;
        GPAP_NUM_COR_PRO	    :=0;
 

-- pap_cod_beneficio_faleceu_atividade  NUMBER(8);

 --pap_flg_faleceu_atividade            VARCHAR2(1);
-- pap_flg_faleceu_acid_doenca          VARCHAR2(1);
-- pap_flg_obito_decorrente_exerc_razao VARCHAR2(1) ;
 
 
        --- Obtenedo regra principal de pens?o por morte.   
         SELECT  RB.SEQ_BEN                           , 
                 PSE.COD_IDE_CLI_SERV                 ,
                 PSE.NUM_MATRICULA                    ,
                 PSE.COD_IDE_REL_FUNC                 ,
                 PSE.COD_ENTIDADE                     ,
                 PSE.FLG_FALECEU_ATIVIDADE            ,
                 PSE.FLG_OBITO_DECORRENTE_EXERC_RAZAO ,
                 PSE.COD_BENEFICIO_FALECEU_ATIVIDADE  ,
                 PSE.FLG_FALECEU_ACID_DOENCA          ,
                 PSE.QTD_TMP_CONTR_DIAS_TOTAL         , 
                 SERV.DAT_OBITO                       ,
                 PSE.DAT_ING_SERV_PUB                 ,          
                 PSE.COD_TAREFA                       ,
                 PSE.COD_REGISTRO
                 INTO   GPAP_SEQBEN                             ,
                          GPAP_COD_IDE_CLI_SERV                 ,
                          GPAP_NUM_MATRICULA    ,
                          GPAP_COD_IDE_REL_FUNC ,
                          GPAP_COD_ENTIDADE     ,                             
                          CGPA_flg_faleceu_atividade            ,
                          CGPA_flg_obito_decorrente_exerc_raza  ,
                          CGPA_cod_beneficio_faleceu_atividade  ,
                          CGPA_flg_faleceu_acid_doenca          ,  
                          CGPA_QTD_TMP_CONTR_DIAS_TOTAL         ,
                          CGPA_DAT_OBITO                        ,
                          CGPA_DAT_ING_SERV_PUB                 ,
                          CGPA_COD_TAREFA                       , 
                          CGPA_COD_REGISTRO                                              
                 FROM TB_REGRA_CONC_BEN              RB  ,
                           TB_REGRA_BENEF            RBE ,
                           TB_NPM_SERVIDOR           PSE ,
                           TB_PESSOA_FISICA          SERV 
            WHERE RB.COD_INS          = PAR_COD_INS                      AND
                  RB.COD_ESTRUTURA    =  1                               AND
                  RB.COD_TIP_REGRA    =  2                               AND 
                  SERV.COD_INS        =  PAR_COD_INS                     AND  
                  SERV.COD_IDE_CLI    = PSE.COD_IDE_CLI_SERV             AND
                  SERV.DAT_OBITO  >=RB.DAT_INI_VIG                       AND   
                  SERV.DAT_OBITO  <=NVL(RB.DAT_FIM_VIG ,SERV.DAT_OBITO ) AND   
                  RBE.COD_INS         = PAR_COD_INS                      AND
                  RBE.SEQ_BEN         = RB.SEQ_BEN                       AND
                  PSE.COD_INS         = PAR_COD_INS                      AND
                  PSE.COD_REGISTRO    = IN_COD_REGISTRO                  AND
                  PSE.COD_ADM_TRA     = IN_COD_ADM_TRA  and
                  RB.SEQ_BEN= 202001;
  

       EXCEPTION
          WHEN NO_DATA_FOUND THEN
                 oErrorCode    := SQLCODE;
                 oErrorMessage := 'SP_CALCULA_REGRA_PENSAO - NENHUMA REGRA ENCONTRADA PARA A DATA DO OBITO';
          
          WHEN OTHERS THEN
               oErrorCode     := SQLCODE;
                oErrorMessage := 'SP_CALCULA_REGRA_PENSAO : OTHERS - '||SQLERRM;
             
       END;
       
        ----------------- Dados para Teste ------------------
/*              CGPA_flg_faleceu_atividade          := 'S' ;
              CGPA_flg_obito_decorrente_exerc_raza:= 'N' ;
              EXISTE_BENEFICIO_APO:=0;
              CGPA_cod_beneficio_faleceu_atividade:=NULL; */

        --6	Media 100%
        --5	Media como se aposentado por invalidez
        --4	Ultimo Salario Contribuic?o
        --3	Composic?o de beneficio de aposentadoria
           
              
       IF oErrorCode   =0 THEN 
                 BEGIN
 
                 CASE
                    ---- Caso: 3 - Media como se aposentado fosse
                    WHEN
                      NVL(CGPA_flg_faleceu_atividade,'N')           = 'S' AND
                      NVL(CGPA_flg_obito_decorrente_exerc_raza,'N') = 'N' AND
                      NVL(CGPA_flg_faleceu_acid_doenca,'N')         = 'N' AND
                      ( CGPA_cod_beneficio_faleceu_atividade IS NULL AND
                        NVL(EXISTE_BENEFICIO_APO,0)              =0
                      ) THEN
                       BEGIN
                         CGPA_IND_TIP_CALC  :='5'; 
                        --- Calculo de 60 % + 2 % por ano
                        CGPA_POR_CALCULO  := TRUNC(CGPA_QTD_TMP_CONTR_DIAS_TOTAL/365)- 20;
                        IF  CGPA_POR_CALCULO > 0 THEN
                           CGPA_POR_CALCULO  := 60 + TRUNC((TRUNC(CGPA_QTD_TMP_CONTR_DIAS_TOTAL/365)- 20)*2);
                        ELSE
                           CGPA_POR_CALCULO  := 60;
                        END IF;
                       
                          SP_LIMPA_ADM_DADOS_RUBRICA (
                              PAR_COD_INS           ,
                              IN_COD_ADM_TRA        ,
                              IN_COD_REGISTRO       ,
                              CGPA_IND_TIP_CALC     ,
                              OERRORCODE            ,
                              OERRORMESSAGE                                     
                          );
                          IF nvl(OERRORCODE,0)  =0 THEN   
                                SP_OBTEM_DADOS_APOSENTADORIA_REGRA3
                                           (PAR_COD_INS           ,
                                            IN_COD_ADM_TRA        ,
                                            CGPA_DAT_OBITO        ,
                                            GPAP_COD_IDE_CLI_SERV ,
                                            GPAP_NUM_MATRICULA    ,
                                            GPAP_COD_IDE_REL_FUNC ,
                                            GPAP_COD_ENTIDADE     , 
                                            100                   ,  
                                            '05'                  , 
                                            CGPA_POR_CALCULO      , 
                                            CGPA_DAT_ING_SERV_PUB ,
                                            CGPA_COD_TAREFA       , 
                                            CGPA_COD_REGISTRO     ,
                                            PAR_COD_USER          , 
                                            GPAP_SEQBEN           ,                                          
                                            OERRORCODE            ,
                                            OERRORMESSAGE        ) ;  
                                CGPA_IND_TIP_COB :='P';                                   
                                CGPA_POR_RETRIB  := CGPA_POR_CALCULO;                        
                          END IF;    
                       END;
                      
                     ---- Caso: 4 - Media 100%
                     WHEN
                      NVL(CGPA_flg_faleceu_atividade,'N')           = 'S' AND
                      NVL(CGPA_flg_obito_decorrente_exerc_raza,'N') = 'N' AND
                      NVL(CGPA_flg_faleceu_acid_doenca,'N')         = 'S' AND                      
                       ( CGPA_cod_beneficio_faleceu_atividade IS NULL AND
                        NVL(EXISTE_BENEFICIO_APO,0)              =0
                      ) THEN
                       BEGIN
                         CGPA_IND_TIP_CALC  :='6';  
                         SP_LIMPA_ADM_DADOS_RUBRICA (
                              PAR_COD_INS           ,
                              IN_COD_ADM_TRA        ,
                              IN_COD_REGISTRO       ,
                              CGPA_IND_TIP_CALC     ,
                              OERRORCODE            ,
                              OERRORMESSAGE                                     
                          );
                          IF nvl(OERRORCODE,0)  =0 THEN                          
 
                                  CGPA_POR_CALCULO  := 100;
                                
                                  SP_OBTEM_DADOS_APOSENTADORIA_REGRA3
                                             (PAR_COD_INS           ,
                                              IN_COD_ADM_TRA        ,
                                              CGPA_DAT_OBITO        ,
                                              GPAP_COD_IDE_CLI_SERV ,
                                              GPAP_NUM_MATRICULA    ,
                                              GPAP_COD_IDE_REL_FUNC ,
                                              GPAP_COD_ENTIDADE     , 
                                              100                   , 
                                              '06'                  ,
                                              CGPA_POR_CALCULO      ,    
                                              CGPA_DAT_ING_SERV_PUB ,  
                                              CGPA_COD_TAREFA       , 
                                              CGPA_COD_REGISTRO     , 
                                              PAR_COD_USER          ,
                                              GPAP_SEQBEN           ,                                                                          
                                              OERRORCODE            ,
                                              OERRORMESSAGE        ) ;      
                             
                                   CGPA_IND_TIP_COB :='I';
                             END IF;
                         END;
                     WHEN  ---- Caso: 2  Ultima contribuicao -Policia civil
                      NVL(CGPA_flg_faleceu_atividade,'N')           = 'S' AND
                      NVL(CGPA_flg_obito_decorrente_exerc_raza,'N') = 'S' AND
                      ( CGPA_cod_beneficio_faleceu_atividade IS NULL AND
                        NVL(EXISTE_BENEFICIO_APO,0)                =0
                      ) THEN
                      
                         BEGIN
                             CGPA_IND_TIP_CALC  :='4'; 
                            SP_LIMPA_ADM_DADOS_RUBRICA (
                                  PAR_COD_INS           ,
                                  IN_COD_ADM_TRA        ,
                                  IN_COD_REGISTRO       ,
                                  CGPA_IND_TIP_CALC     ,
                                  OERRORCODE            ,
                                  OERRORMESSAGE                                     
                              );
                              IF nvl(OERRORCODE,0)  =0 THEN                         
                                     SP_OBTEM_DADOS_APOSENTADORIA_REGRA2
                                             (PAR_COD_INS           ,
                                              IN_COD_ADM_TRA        ,
                                              OERRORCODE            ,
                                              OERRORMESSAGE        ) ;  
                                  CGPA_IND_TIP_COB :='I';                                   
                                  CGPA_POR_RETRIB  :=100;
                              END IF;
                            
                           END;

                    WHEN  ---- Caso: 1 - Beneficio de Aposentadoria
                        ----  N?o administrados 
                      NVL(CGPA_flg_faleceu_atividade,'N')    = 'N' AND                    
                      NVL(EXISTE_BENEFICIO_APO,0)            =0
                       THEN
                       BEGIN
                          CGPA_IND_TIP_COB :='I';                                   
                          CGPA_POR_RETRIB  :=100;                         
                       
                          CGPA_IND_TIP_CALC  :='3'; 
                          SP_LIMPA_ADM_DADOS_RUBRICA (
                                PAR_COD_INS           ,
                                IN_COD_ADM_TRA        ,
                                IN_COD_REGISTRO       ,
                                CGPA_IND_TIP_CALC     ,
                                OERRORCODE            ,
                                OERRORMESSAGE                                     
                            );
                         END;
                   WHEN  ---- Caso: 1 - Beneficio de Aposentadoria
                        NVL(CGPA_flg_faleceu_atividade,'N')           = 'N' AND                    
                        NVL(EXISTE_BENEFICIO_APO,0)                !=0
                        THEN
                       BEGIN
                          CGPA_IND_TIP_CALC  :='3'; 
                          CGPA_IND_TIP_COB   :='I';                                   
                          CGPA_POR_RETRIB    :=100;                               
                          
                          SP_LIMPA_ADM_DADOS_RUBRICA (
                                PAR_COD_INS           ,
                                IN_COD_ADM_TRA        ,
                                IN_COD_REGISTRO       ,
                                CGPA_IND_TIP_CALC     ,
                                OERRORCODE            ,
                                OERRORMESSAGE                                     
                            );
                            IF nvl(OERRORCODE,0)  =0 THEN                          
                                  SP_OBTEM_DADOS_APOSENTADORIA_REGRA1
                                             (PAR_COD_INS           ,
                                              IN_COD_ADM_TRA        ,
                                              CGPA_cod_beneficio_faleceu_atividade ,
                                              CGPA_IND_TIP_COB      ,
                                              CGPA_POR_RETRIB       ,
                                              OERRORCODE            ,
                                              OERRORMESSAGE        ) ;
                            END IF;
                       END;
                  ELSE 
                      CGPA_IND_TIP_CALC  :='E';  
                  END CASE;
                
        IF  CGPA_IND_TIP_CALC <>'E'  and nvl(OERRORCODE,0)  =0 THEN 
 

             ----------- Controle de Dados  -----------                         
                GPAP_seq                :=GPAP_SEQBEN          ;--  PAP_SEQ            -> Numero Sequencial 
                GPAP_cod_ins            :=PAR_COD_INS          ;--  PAP_COD_INS        -> Codigo do Instituto
                GPAP_cod_ide_cli_serv   :=GPAP_COD_IDE_CLI_SERV;--  PAP_IDE_CLI        -> COD_IDE_CLI do servidor
                GPAP_cod_adm_tra        :=IN_COD_ADM_TRA       ;--  PAP_COD_ADM_TRA    -> N?mero do Protocolo
                GPAP_FEC_PRO            :=GPAP_FEC_PRO	       ;--  PAP_FEC_PRO        -> Data de Criacao do registro
                GPAP_NUM_COR_PRO        :=0                    ;--  PAP_NUM_COR_PRO    -> ??
                GPAP_ind_tip_tra        :='NPM'                ;--  PAP_IND_TIP_TRA    -> C?digo do tipo de fluxo, no nosso caso ? NPM
                GPAP_ind_tip_reg        :=' '                  ;--  PAP_IND_TIP_REG    -> Tipo de Regimento () 
                GPAP_ind_tip_cob        :=CGPA_IND_TIP_COB     ;--  PAP_IND_TIP_COB    -> Tipo de Cobertura (P - Proporcional, I - Integral)
                GPAP_ind_pro_apr        :='S'                  ;--  PAP_IND_PRO_APR    -> 'INDICADOR SI PROPUESTA ESTA APROBADO O VALIDA (S,N)'
                GPAP_tie_fal_ano        := 0                   ;--  PAP_TIE_FAL_ANO    -> 'TIEMPO QUE FALTA PARA BENEFICIO EN ANOS'
                GPAP_tie_fal_mes        := 0                   ;--  PAP_TIE_FAL_MES    -> 'TIEMPO QUE FALTA PARA BENEFICIO EN MESES'
                GPAP_tie_fal_dia        := 0                   ;--  PAP_TIE_FAL_DIA    -> 'TIEMPO QUE FALTA PARA BENEFICIO EN DIAS'
                GPAP_mon_por_pro        := 0                   ;--  PAP_MON_POR_PRO    -> 'MONTO DEL PORCENTAJE DE PENSION PROPORCIONAL'
                GPAP_mon_est_pro        := 0                   ;--  PAP_MON_EST_PRO    -> 'MONTO ESTIMADO DEL PROVENTO EN REALES'
                GPAP_cod_car            := 0                   ;--  PAP_COD_CAR        -> 'CODIGO DEL CARGO DEL SERVIDOR'
                GPAP_cod_fun            := 0                   ;--  PAP_COD_FUN        -> 'CODIGO DE LA FUNCION DEL SERVIDOR'
                GPAP_cod_niv            := 0                   ;--  PAP_COD_NIV        -> 'CODIGO DEL NIVEL DEL SERVIDOR'
                GPAP_ide_emp            := 0                   ;--  PAP_IDE_EMP        -> 'IDENTIFICACION DEL EMPLEADOR DEL SERVIDOR'
                GPAP_ind_ace_ser        := 'S'                 ;--  PAP_IND_ACE_SER    -> 'INDICADOR DE ACEPTACION DEL SERVIDOR (S,N)'
                GPAP_fec_ace_ser        := GPAP_FEC_PRO        ;--  PAP_FEC_ACE_SER    -> 'FECHA DE ACEPTACION DEL SERVIDOR'
                GPAP_fec_ing            := GPAP_FEC_PRO        ;--  PAP_FEC_ING        -> 'FECHA DE INGRESO DE REGISTRO'
                GPAP_fec_ult_man        := GPAP_FEC_PRO        ;--  PAP_FEC_ULT_MAN    -> 'FECHA DE ULTIMA MANTENCION DE REGISTRO'
                GPAP_nom_usu_man        := PAR_COD_USER        ;--  PAP_NOM_USU_MAN    -> 'NOMBRE USUARIO DE MANTENCION'
                GPAP_nom_pro_man        := PAR_COD_USER        ;--  PAP_NOM_PRO_MAN    -> 'NOMBRE PROCESO DE MANTENCION'
                GPAP_obs                :=' '                  ;--  PAP_OBS            -> 'MOTIVO PELO QUAL NAO PODE SE APOSENTAR'
                GPAP_tip_tra_fim        :='NPM'                ;--  PAP_TIP_TRA_FIM    -> 'TIPO DE BENEFICIO FINAL'
                GPAP_COD_BEN            :=GPAP_SEQBEN          ;--  PAP_COD_BEN        -> 'CODIGO DA REGRA APLICADA - BENCAL'
                GPAP_por_exced_teto     :=0                    ;--  PAP_POR_EXCED_TETO -> 'PORCENTUAL APLICADO NO EXCEDIDO DO TETO'
                GPAP_val_teto           :=0                    ;--  PAP_VAL_TETO       -> 'VALOR DO TETO'
                GPAP_val_exced_desc     :=0                    ;--  PAP_VAL_EXCED_DESC -> 'VALOR EXCEDIDO DO TETO COM DESCONTO'
                GPAP_val_exced_teto     :=0                    ;--  PAP_VAL_EXCED_TETO -> 'VALOR EXCEDIDO DO TETO'
                GPAP_val_ben_calc       :=0                    ;--  PAP_VAL_BEN_CALC   -> 'VALOR TOTAL DE OUTROS BENEFICIOS CALCULADOS'
                GPAP_val_desc_calc      :=0                    ;--  PAP_VAL_DESC_CALC  -> 'VALOR DE DESCONTO JA CALCULADO PRA OUTROS BENEFICIOS'
                GPAP_por_retrib         :=CGPA_POR_RETRIB      ; -- PAP_POR_RETRIB     -> 'Porcentagem da retribuic?o paga ao servidor'
                GPAP_ind_tip_calc       :=CGPA_IND_TIP_CALC    ;--  PAP_IND_TIP_CALC   -> 'tipo de calculo - cod_num = 3019'
                GPAP_dat_invalidez      :=NULL                 ;--  PAP_DAT_INVALIDEZ
                GPAP_num_laudo_medico   :=NULL                 ;--  PAP_NUM_LAUDO_MEDICO
            --------------------------------------------
                           SP_INCLUE_SIMPAP_PENSAO(
                                    GPAP_SEQBEN         ,
                                    GPAP_cod_ins         ,
                                    GPAP_cod_ide_cli_serv,
                                    GPAP_cod_adm_tra     ,
                                    GPAP_FEC_PRO         ,
                                    GPAP_NUM_COR_PRO     ,
                                    GPAP_ind_tip_tra     ,
                                    GPAP_ind_tip_reg     ,
                                    GPAP_ind_tip_cob     ,
                                    GPAP_ind_pro_apr     ,
                                    GPAP_tie_fal_ano     ,
                                    GPAP_tie_fal_mes     ,
                                    GPAP_tie_fal_dia     ,
                                    GPAP_mon_por_pro     ,
                                    GPAP_mon_est_pro     ,
                                    GPAP_cod_car         ,
                                    GPAP_cod_fun         ,
                                    GPAP_cod_niv         ,
                                    GPAP_ide_emp         ,
                                    GPAP_ind_ace_ser     ,
                                    GPAP_fec_ace_ser     ,
                                    GPAP_fec_ing         ,
                                    GPAP_fec_ult_man     ,
                                    GPAP_nom_usu_man     , 
                                    GPAP_nom_pro_man     ,
                                    GPAP_obs             ,
                                    GPAP_tip_tra_fim     ,
                                    GPAP_COD_BEN         ,
                                    GPAP_por_exced_teto  ,
                                    GPAP_val_teto        ,
                                    GPAP_val_exced_desc  ,
                                    GPAP_val_exced_teto  ,
                                    GPAP_val_ben_calc    ,
                                    GPAP_val_desc_calc   ,
                                    GPAP_por_retrib      ,
                                    GPAP_ind_tip_calc    ,
                                    GPAP_dat_invalidez   ,
                                    GPAP_num_laudo_medico,
                                    CGPA_flg_faleceu_atividade            ,
                                    CGPA_flg_obito_decorrente_exerc_raza  ,
                                    CGPA_cod_beneficio_faleceu_atividade  ,
                                    CGPA_flg_faleceu_acid_doenca          ,                                      
                                    oErrorCode           ,
                                    oErrorMessage
                          );

              IF NVL(oErrorCode,0)  =0 THEN
                 SP_EMQUADRA_BENEFICIARIOS (PAR_COD_INS    ,
                                           IN_COD_ADM_TRA  ,
                                           SEQBEN          ,
                                           PAR_COD_REGISTRO,
                                           OERRORCODE      ,
                                           OERRORMESSAGE  ); 
              END IF;

            ELSE  
                oErrorCode    := 54; 
                oErrorMessage := 'Sem Regra de identificada vinculo ('||to_char( GPAP_COD_IDE_REL_FUNC ) ||')' ;
               null;
            END IF;
         END;
 
    END IF;
   

 
    EXCEPTION
      WHEN v_erro THEN
         oErrorCode    := 4;--P_CODERRO;
         oErrorMessage := 'SP_CALCULA_REGRA_PENSAO : V_ERRO '|| oErrorCode;
       

      WHEN OTHERS THEN
       oErrorCode    := SQLCODE;
       oErrorMessage := 'SP_CALCULA_REGRA_PENSAO : OTHERS '|| SQLERRM;
      
/*
EXCEPTION
       WHEN OTHERS THEN
      error          := SQLCODE;
      errorMessage   := SQLERRM || P_MSGERRO;
*/
END SP_CALCULA_REGRA_PENSAO;

procedure SP_INCLUE_SIMPAP_PENSAO (
                          FGPAP_seq              IN NUMBER   ,
                          FGPAP_cod_ins          IN NUMBER   ,
                          FGPAP_cod_ide_cli_serv IN VARCHAR2 ,
                          FGPAP_cod_adm_tra      IN VARCHAR2 ,
                          FGPAP_FEC_PRO          IN DATE,
                          FGPAP_NUM_COR_PRO      IN NUMBER,
                          FGPAP_ind_tip_tra      IN CHAR  ,
                          FGPAP_ind_tip_reg      IN VARCHAR2  ,
                          FGPAP_ind_tip_cob      IN VARCHAR2  ,
                          FGPAP_ind_pro_apr      IN VARCHAR2 ,
                          FGPAP_tie_fal_ano      IN NUMBER   ,
                          FGPAP_tie_fal_mes      IN NUMBER   ,
                          FGPAP_tie_fal_dia      IN NUMBER   ,
                          FGPAP_mon_por_pro      IN NUMBER   ,
                          FGPAP_mon_est_pro      IN NUMBER   ,
                          FGPAP_cod_car          IN NUMBER   ,
                          FGPAP_cod_fun          IN NUMBER   ,
                          FGPAP_cod_niv          IN NUMBER   ,
                          FGPAP_ide_emp          IN VARCHAR2 ,
                          FGPAP_ind_ace_ser      IN VARCHAR2  ,
                          FGPAP_fec_ace_ser      IN DATE,
                          FGPAP_fec_ing          IN DATE,
                          FGPAP_fec_ult_man      IN DATE,
                          FGPAP_nom_usu_man      IN VARCHAR2 ,
                          FGPAP_nom_pro_man      IN VARCHAR2 ,
                          FGPAP_obs              IN VARCHAR2 ,
                          FGPAP_tip_tra_fim      IN CHAR     ,
                          FGPAP_COD_BEN          IN NUMBER   ,
                          FGPAP_por_exced_teto   IN NUMBER   ,
                          FGPAP_val_teto         IN NUMBER   ,
                          FGPAP_val_exced_desc   IN NUMBER   ,
                          FGPAP_val_exced_teto   IN NUMBER   ,
                          FGPAP_val_ben_calc     IN NUMBER   ,
                          FGPAP_val_desc_calc    IN NUMBER   ,
                          FGPAP_por_retrib       IN NUMBER   ,
                          FGPAP_ind_tip_calc     IN VARCHAR2 ,
                          FGPAP_dat_invalidez    IN DATE     ,
                          FGPAP_num_laudo_medico IN NUMBER   ,
                          p_flg_faleceu_atividade           IN  VARCHAR2, 
                          p_flg_obito_decorrente_exerc_raza IN  VARCHAR2, 
                          P_cod_beneficio_faleceu_atividade IN  NUMBER  ,
                          P_flg_faleceu_acid_doenca         IN  VARCHAR2,                            
                          P_ERRORCODE           OUT NUMBER  ,
                          P_MENSAJE             OUT VARCHAR2
                  ) IS

    QTD_NUM_COR_PRO NUMBER;
  BEGIN
      P_errorcode := 0;
      P_Mensaje   :='';
 
    
      BEGIN
           SELECT NVL(COUNT(*),0)  +1  
           INTO   QTD_NUM_COR_PRO
           FROM SIMPAP PAP
                WHERE PAP.PAP_COD_INS     = FGPAP_COD_INS    AND
                      PAP.PAP_COD_ADM_TRA = FGPAP_COD_ADM_TRA; 
        EXCEPTION
        WHEN OTHERS THEN
            p_errorcode:= -1 ;
            p_mensaje:= SQLERRM||'Erro ao preparar ingresso da Regra ';

      END;
         
       IF NVL(p_errorcode,0)  =  0 THEN 
              
            INSERT INTO SIMPAP
             (
                  PAP_SEQ ,
                  PAP_COD_INS ,
                  PAP_IDE_CLI ,
                  PAP_COD_ADM_TRA ,
                  PAP_FEC_PRO ,
                  PAP_NUM_COR_PRO ,
                  PAP_IND_TIP_TRA ,
                  PAP_IND_TIP_REG ,
                  PAP_IND_TIP_COB ,
                  PAP_IND_PRO_APR ,
                  PAP_TIE_FAL_ANO ,
                  PAP_TIE_FAL_MES ,
                  PAP_TIE_FAL_DIA ,
                  PAP_MON_POR_PRO ,
                  PAP_MON_EST_PRO ,
                  PAP_COD_CAR ,
                  PAP_COD_FUN ,
                  PAP_COD_NIV ,
                  PAP_IDE_EMP ,
                  PAP_IND_ACE_SER ,
                  PAP_FEC_ACE_SER ,
                  PAP_FEC_ING ,
                  PAP_FEC_ULT_MAN ,
                  PAP_NOM_USU_MAN ,
                  PAP_NOM_PRO_MAN ,
                  PAP_OBS ,
                  PAP_TIP_TRA_FIM ,
                  PAP_COD_BEN ,
                  PAP_POR_EXCED_TETO  ,
                  PAP_VAL_TETO  ,
                  PAP_VAL_EXCED_DESC  ,
                  PAP_VAL_EXCED_TETO  ,
                  PAP_VAL_BEN_CALC  ,
                  PAP_VAL_DESC_CALC ,
                  PAP_POR_RETRIB  ,
                  PAP_IND_TIP_CALC  ,
                  PAP_DAT_INVALIDEZ ,
                  PAP_NUM_LAUDO_MEDICO,  
                  PAP_FLG_FALECEU_ATIVIDADE            , 
                  PAP_FLG_OBITO_DECORRENTE_EXERC_RAZAO ,
                  PAP_COD_BENEFICIO_FALECEU_ATIVIDADE  ,
                  PAP_FLG_FALECEU_ACID_DOENCA           
             
             )
            
              VALUES
                        (   FGPAP_SEQ          ,
                            FGPAP_COD_INS      ,
                            FGPAP_COD_IDE_CLI_SERV ,
                            FGPAP_COD_ADM_TRA  ,
                            FGPAP_FEC_PRO      ,
                            QTD_NUM_COR_PRO    , --FGPAP_NUM_COR_PRO  ,
                            FGPAP_IND_TIP_TRA  ,
                            FGPAP_IND_TIP_REG  ,
                            FGPAP_IND_TIP_COB  ,
                            FGPAP_IND_PRO_APR  ,
                            FGPAP_TIE_FAL_ANO  ,
                            FGPAP_TIE_FAL_MES  ,
                            FGPAP_TIE_FAL_DIA  ,
                            FGPAP_MON_POR_PRO  ,
                            FGPAP_MON_EST_PRO  ,
                            FGPAP_COD_CAR      ,
                            FGPAP_COD_FUN      ,
                            FGPAP_COD_NIV      ,
                            FGPAP_IDE_EMP      ,
                            FGPAP_IND_ACE_SER  ,
                            FGPAP_FEC_ACE_SER  ,
                            FGPAP_FEC_ING      ,
                            FGPAP_FEC_ULT_MAN  ,
                            FGPAP_NOM_USU_MAN  ,
                            FGPAP_NOM_PRO_MAN  ,
                            FGPAP_OBS          ,
                            FGPAP_TIP_TRA_FIM  ,
                            FGPAP_COD_BEN      ,
                            FGPAP_POR_EXCED_TETO ,
                            FGPAP_VAL_TETO       ,
                            FGPAP_VAL_EXCED_DESC ,
                            FGPAP_VAL_EXCED_TETO ,
                            FGPAP_VAL_BEN_CALC   ,
                            FGPAP_VAL_DESC_CALC  ,
                            FGPAP_POR_RETRIB     ,
                            FGPAP_IND_TIP_CALC   ,
                            FGPAP_DAT_INVALIDEZ  ,
                            FGPAP_NUM_LAUDO_MEDICO ,
                            P_FLG_FALECEU_ATIVIDADE            , 
                            P_FLG_OBITO_DECORRENTE_EXERC_RAZA  , 
                            P_COD_BENEFICIO_FALECEU_ATIVIDADE  ,
                            P_FLG_FALECEU_ACID_DOENCA                                       
            );
      ELSE
           p_errorcode:= 50;
      END IF;
    EXCEPTION
      WHEN OTHERS THEN
       p_errorcode:= 51;
       p_mensaje:= SQLERRM||'Erro al ingressar Regra de Calculo.';
 
  END  SP_INCLUE_SIMPAP_PENSAO;
  
  PROCEDURE SP_LIMPA_ADM_DADOS_PENSAO
  (
         P_COD_INS     IN NUMBER       ,
         P_COD_ADM_TRA IN VARCHAR2     ,
         P_COD_REGITRO IN NUMBER       ,
         P_COD_ERRO    OUT NUMBER      ,
         P_MSG_ERRO    OUT VARCHAR2
  ) IS
  W_Existe_calculo number :=0;
  BEGIN
     
     DELETE FROM SIMPAP PAP
     WHERE PAP.PAP_COD_INS     = P_COD_INS AND
           PAP.PAP_COD_ADM_TRA = P_COD_ADM_TRA; 


    SELECT COUNT(*) 
           INTO W_Existe_calculo 
     FROM  TB_COMP_SAL_TRA COMP
     WHERE COMP.COD_INS     = P_COD_INS AND
           COMP.COD_ADM_TRA = P_COD_ADM_TRA; 

                         
     DELETE FROM TB_MEDIA_ARITMETICA_NL  MEDH 
     WHERE  MEDH.COD_INS          =  P_COD_INS     AND
            MEDH.COD_ADM_TRA      =  P_COD_ADM_TRA   AND
            MEDH.COD_REGISTRO     =  PAR_COD_REGISTRO  ;
            
    DELETE FROM TB_HMEDIA_ARITMETICA_NL  MEDH 
     WHERE  MEDH.COD_INS          =  P_COD_INS      AND
            MEDH.COD_ADM_TRA      =  P_COD_ADM_TRA  AND
            MEDH.COD_REGISTRO    =  PAR_COD_REGISTRO  ;
                        
                 
      EXCEPTION
      
        WHEN OTHERS THEN
          P_COD_ERRO   := -1;
          P_MSG_ERRO   := 'Erro na limpeza de dados antes  do Calculo';
     
  END;
   
  PROCEDURE SP_LIMPA_ADM_MEDIA 
  (
         P_COD_INS      IN NUMBER       ,
         P_COD_ADM_TRA  IN VARCHAR2     ,
         P_COD_REGISTRO IN NUMBER       ,
         P_COD_ERRO     OUT NUMBER      ,
         P_MSG_ERRO     OUT VARCHAR2
  ) IS
  W_Existe_calculo number :=0;
  BEGIN
     
   

    SELECT COUNT(*) 
           INTO W_Existe_calculo 
     FROM  TB_COMP_SAL_TRA COMP
     WHERE COMP.COD_INS     = P_COD_INS AND
           COMP.COD_ADM_TRA = P_COD_ADM_TRA; 

                         
     DELETE FROM TB_MEDIA_ARITMETICA_NL  MEDH 
     WHERE  MEDH.COD_INS          =  P_COD_INS     AND
            MEDH.COD_ADM_TRA      =  P_COD_ADM_TRA AND
            MEDH.COD_REGISTRO     =  P_COD_REGISTRO  ;
            
    DELETE FROM TB_HMEDIA_ARITMETICA_NL  MEDH 
     WHERE  MEDH.COD_INS          =  P_COD_INS      AND
            MEDH.COD_ADM_TRA      =  P_COD_ADM_TRA  AND
            MEDH.COD_REGISTRO    =   P_COD_REGISTRO  ;

        
           DELETE FROM TB_COMP_SAL_TRA COMP
           WHERE COMP.COD_INS     = P_COD_INS AND
                 COMP.COD_ADM_TRA = P_COD_ADM_TRA; 
                      
                 
           DELETE FROM TB_COMP_SAL_TRA_HIST COMPH 
           WHERE COMPH.COD_INS          =  P_COD_INS     AND
                 COMPH.COD_ADM_TRA      =  P_COD_ADM_TRA AND
                 COMPH.REG_COD_REGISTRO =  P_COD_REGISTRO ;                        
    
                 
      EXCEPTION
      
        WHEN OTHERS THEN
          P_COD_ERRO   := -1;
          P_MSG_ERRO   := 'Erro na limpeza de dados antes  do Calculo';
     
  END;
  
 PROCEDURE SP_LIMPA_ADM_DADOS_RUBRICA
  (
         P_COD_INS       IN NUMBER       ,
         P_COD_ADM_TRA   IN VARCHAR2     ,
         P_COD_REGITRO   IN NUMBER       ,
         P_IND_TIP_CALC  IN CHAR         ,
         P_COD_ERRO      OUT NUMBER      ,
         P_MSG_ERRO      OUT VARCHAR2
  ) IS
  W_Existe_calculo number :=0;
  BEGIN
 

    SELECT COUNT(*) 
           INTO W_Existe_calculo 
     FROM  TB_COMP_SAL_TRA COMP
     WHERE COMP.COD_INS     = P_COD_INS AND
           COMP.COD_ADM_TRA = P_COD_ADM_TRA; 
 
     IF W_Existe_calculo =0 OR  
       ( P_IND_TIP_CALC IN  ('5','6') ) 
         THEN 
           DELETE FROM TB_COMP_SAL_TRA COMP
           WHERE COMP.COD_INS     = P_COD_INS AND
                 COMP.COD_ADM_TRA = P_COD_ADM_TRA; 
                      
                 
           DELETE FROM TB_COMP_SAL_TRA_HIST COMPH 
           WHERE COMPH.COD_INS          =  P_COD_INS     AND
                 COMPH.COD_ADM_TRA      =  P_COD_ADM_TRA AND
                 COMPH.REG_COD_REGISTRO =  PAR_COD_REGISTRO ;
     END IF;    
                 
      EXCEPTION
      
        WHEN OTHERS THEN
          P_COD_ERRO   := -1;
          P_MSG_ERRO   := 'Erro na limpeza de dados antes  do Calculo Rubricas';
     
  END;
  
  Procedure SP_OBTEM_DADOS_APOSENTADORIA_REGRA1
                                     (P_COD_INS           IN NUMBER   ,
                                      P_COD_ADM_TRA       IN VARCHAR  ,
                                      P_COD_BENEFICIO_APO  IN NUMBER  ,
                                      P_IND_TIP_COB    OUT VARCHAR,
                                      P_POR_RETRIB     OUT NUMBER,
                                      OERRORCODE       OUT NUMBER,
                                      OERRORMESSAGE    OUT VARCHAR) AS

 seg_rubrica         number :=0; 
 W_Existe_calculo    number :=0;  
 BEGIN
 
 
   
 
    BEGIN  
      
         SELECT COUNT(*) 
           INTO W_Existe_calculo 
           FROM  TB_COMP_SAL_TRA COMP
           WHERE COMP.COD_INS     = P_COD_INS AND
                 COMP.COD_ADM_TRA = P_COD_ADM_TRA; 

        
    
               IF P_COD_BENEFICIO_APO IS NOT NULL THEN
                   P_POR_RETRIB:=0;      
                   SELECT  
                        CONC.VAL_PERCENT_BEN ,
                        CASE     TRUNC(NVL(CONC.VAL_PERCENT_BEN,0)) 
                           WHEN   100 THEN
                                   'I'
                           WHEN    0  THEN
                                   'I'
                           ELSE
                                   'P'
                           END
                       
                      INTO   P_POR_RETRIB ,P_IND_TIP_COB  
                     FROM TB_CONCESSAO_BENEFICIO  CONC
                     WHERE  CONC.COD_INS       = P_COD_INS 
                       AND  CONC.COD_BENEFICIO = P_COD_BENEFICIO_APO;
                       
                     FOR  CONCESSAO_BEN IN (
                          
                         SELECT 
                            --- Bloco de controle 
                             CONC.cod_ins
                            ,CONC.cod_beneficio
                            ,CONC.cod_entidade
                            ,CONC.cod_ide_cli_serv
                            ,CONC.num_matricula
                            ,CONC.cod_ide_rel_func
                            ---- Bloco proincipal                   
                            ,COMP.cod_fcrubrica
                            ,COMP.seq_vig
                            ,COMP.val_fixo
                            ,COMP.val_porc
                            ,COMP.cod_referencia
                            ,COMP.val_inidade
                            ,COMP.val_porc2
                            ,COMP.cod_tabela
                            ,COMP.cod_funcao
                            ,COMP.cod_cargo
                            ,COMP.dat_incorp
                            ,COMP.cod_referencia_2
                            ,COMP.cod_cargo_2
                            ----- Bloco Secundario 
         
                            ----- Bloco de Auditoria 
         
         
                         FROM TB_COMPOSICAO_BEN      COMP ,
                              TB_CONCESSAO_BENEFICIO CONC
                         WHERE COMP.COD_INS                =  P_COD_INS  AND
                               COMP.COD_BENEFICIO          =  P_COD_BENEFICIO_APO AND
                               CONC.COD_INS                =  P_COD_INS           AND
                               CONC.COD_BENEFICIO          =  P_COD_BENEFICIO_APO AND
                               nvl(COMP.FLG_STATUS,'V')    = 'V' AND
                               NVL(COMP.DAT_FIM_VIG,SYSDATE)  >= SYSDATE 
                           
                     
                     )
                     LOOP
                        seg_rubrica:=seg_rubrica+1;
                    IF W_Existe_calculo =0 THEN 
                        INSERT INTO TB_COMP_SAL_TRA 
                         (
                            --- Bloco de controle 
                               cod_ins
                              ,cod_ide_cli
                              ,num_matricula
                              ,cod_ide_rel_func
                              ,cod_entidade 
                              ,cod_adm_tra

                            ---- Bloco proincipal                   
                                ,cod_cargo_incorp
                                ,cod_cargo_2
                                ,cod_rubrica
                                ,cod_fcrubrica
                                ,cod_funcao
                                ,cod_ref_pad_venc
                                ,num_refe_calc
                                ,cod_tabela
                                ,dat_incorp
                                ,num_seq_vig
                                ,num_seq
                                ,val_rubrica
                                ,val_unidade
                                ,val_por
                                ,val_prop_tram 


         
                            ----- Bloco de Auditoria 
                                 ,dat_ing
                                 ,dat_ult_atu
                                 ,nom_usu_ult_atu
                                 ,nom_pro_ult_atu

                          )
                          VALUES
                          (
                             --- Bloco de controle 
                                CONCESSAO_BEN.cod_ins
                               ,CONCESSAO_BEN.cod_ide_cli_serv
                               ,CONCESSAO_BEN.num_matricula
                               ,CONCESSAO_BEN.cod_ide_rel_func
                               ,CONCESSAO_BEN.cod_entidade 
                               ,P_COD_ADM_TRA 
                            ---- Bloco proincipal                   
                                ,CONCESSAO_BEN.cod_cargo
                                ,CONCESSAO_BEN.cod_cargo_2
                                ,CONCESSAO_BEN.cod_fcrubrica
                                ,CONCESSAO_BEN.cod_fcrubrica
                                ,CONCESSAO_BEN.cod_funcao
                                ,CONCESSAO_BEN.cod_referencia
                                ,CONCESSAO_BEN.cod_referencia_2
                                ,CONCESSAO_BEN.cod_tabela
                                ,CONCESSAO_BEN.dat_incorp
                                ,1
                                ,seg_rubrica
                                ,nvl(CONCESSAO_BEN.Val_fixo,0)
                                ,CONCESSAO_BEN.val_inidade
                                ,CONCESSAO_BEN.val_porc
                                ,CONCESSAO_BEN.val_porc2


                            ----- Bloco Secundario 
                 
                            ----- Bloco de Auditoria                  
                                 , sysdate -- dat_ing
                                 , sysdate --dat_ult_atu
                                 , PAR_COD_USER  --nom_usu_ult_atu
                                 ,'Calculo de pesao' --nom_pro_ult_atu                 
                          );
                        
                       END IF;                                                                        
                     END LOOP;
                     
                     ---- Atualiza % da aposentadoria legado
                      UPDATE TB_NPM_SERVIDOR NPM_SER
                             SET NPM_SER.PERC_BENEFICIO_FALECEU_ATIVIDADE =P_POR_RETRIB
                             WHERE NPM_SER.COD_INS      =   P_COD_INS      AND
                                   NPM_SER.COD_ADM_TRA  =   P_COD_ADM_TRA  AND
                                   NPM_SER.COD_REGISTRO =   PAR_COD_REGISTRO ;
                
               ELSE
                         OERRORCODE       :=81;
                         OERRORMESSAGE    :='Erro ao obter dados da aposentadoaria origem';
               END IF;
           
     EXCEPTION
        WHEN OTHERS THEN
                 OERRORCODE       :=89;
                 OERRORMESSAGE    :=SQLERRM;--'Erro ao obter dados da aposentadoaria origem';

  END;
  END;
 
 Procedure SP_OBTEM_DADOS_APOSENTADORIA_REGRA2
                                     (P_COD_INS             IN  NUMBER ,
                                      P_COD_ADM_TRA         IN  VARCHAR,
                                      OERRORCODE            OUT NUMBER,
                                      OERRORMESSAGE         OUT VARCHAR) AS
 CDAD_PER_ULT_FICHA VARCHAR2(6);
 W_Existe_calculo    number :=0;  
 BEGIN
     BEGIN  
      
         SELECT COUNT(*) 
           INTO W_Existe_calculo 
           FROM  TB_COMP_SAL_TRA COMP
           WHERE COMP.COD_INS     = P_COD_INS AND
                 COMP.COD_ADM_TRA = P_COD_ADM_TRA; 

             SELECT MAX( LPAD(FINA.DAT_ANO,4,0)||LPAD(FINA.DAT_MES,2,0)) 
            INTO CDAD_PER_ULT_FICHA
             FROM    TB_NPM_SERVIDOR        SERP,
                    TB_FICHA_FINANCEIRA    FINA,
                    TB_PESSOA_FISICA       PF   
              WHERE SERP.COD_INS         = P_COD_INS              AND
                    SERP.COD_ADM_TRA     = P_COD_ADM_TRA          AND
                    SERP.COD_REGISTRO    = PAR_COD_REGISTRO       AND
                    FINA.COD_INS         = P_COD_INS              AND
                    FINA.COD_IDE_CLI     = SERP.COD_IDE_CLI_SERV  AND
                    FINA.NUM_MATRICULA   = SERP.NUM_MATRICULA     AND
                    FINA.COD_IDE_REL_FUNC= SERP.COD_IDE_REL_FUNC  AND
                    FINA.COD_ENTIDADE    = SERP.COD_ENTIDADE      AND
                    PF.COD_INS           = P_COD_INS              AND
                    PF.COD_IDE_CLI       = SERP.COD_IDE_CLI_SERV  AND
                    LPAD(FINA.DAT_ANO,4,0)||LPAD(FINA.DAT_MES,2,0) <=TO_CHAR(PF.DAT_OBITO,'YYYYMM'); 
                     

            
              FOR  FICHA_FINANCEIRA_ATIVO IN (
                 SELECT 
                    --- Bloco de controle 
                    SERP.cod_ins
                    
                    ,SERP.cod_entidade
                    ,SERP.cod_ide_cli_serv
                    ,SERP.num_matricula
                    ,SERP.cod_ide_rel_func
                    ---- Bloco proincipal   
                    ,NULL cod_cargo
                    ,NULL cod_cargo_2
                    ,rue.cod_rubrica_sistema  cod_fcrubrica
                    ,NULL cod_funcao
                    ,NULL cod_referencia
                    ,NULL cod_referencia_2
                    ,NULL cod_tabela
                    ,NULL dat_incorp
                    ,FIRU.NUM_SEQ_VIG  AS seq_vig
                    ,FIRU.VAL_RUBRICA  AS Val_fixo
                    ,NULL val_inidade
                    ,NULL val_porc
                    ,NULL val_porc2
                    
   
                    ----- Bloco Secundario 
 
                    ----- Bloco de Auditoria 
 
             FROM    TB_NPM_SERVIDOR        SERP,
                    TB_FICHA_FINANCEIRA    FINA ,
                    TB_PESSOA_FISICA       PF   ,
                    TB_FICHA_FIN_RUBRICA   FIRU ,
                    TB_RUBRICA_ENTIDADE    RUE
              WHERE SERP.COD_INS         = P_COD_INS              AND
                    SERP.COD_ADM_TRA     = P_COD_ADM_TRA          AND
                    SERP.COD_REGISTRO    = PAR_COD_REGISTRO       AND
                    FINA.COD_INS         = P_COD_INS              AND
                    FINA.COD_IDE_CLI     = SERP.COD_IDE_CLI_SERV  AND
                    FINA.NUM_MATRICULA   = SERP.NUM_MATRICULA     AND
                    FINA.COD_IDE_REL_FUNC= SERP.COD_IDE_REL_FUNC  AND
                    FINA.COD_ENTIDADE    = SERP.COD_ENTIDADE      AND
                    PF.COD_INS           = P_COD_INS              AND
                    PF.COD_IDE_CLI       = SERP.COD_IDE_CLI_SERV  AND
                    FIRU.COD_INS         = P_COD_INS              AND
                    FIRU.COD_IDE_CLI     = SERP.COD_IDE_CLI_SERV  AND
                    FIRU.COD_ENTIDADE    = SERP.COD_ENTIDADE      AND
                    FIRU.NUM_MATRICULA   = SERP.NUM_MATRICULA     AND
                    FIRU.COD_IDE_REL_FUNC= SERP.COD_IDE_REL_FUNC  AND
                    FIRU.DAT_MES         = FINA.DAT_MES           AND
                    FIRU.DAT_ANO         = FINA.DAT_ANO           AND
                    RUE.COD_INS          = FIRU.COD_INS           AND
                    RUE.COD_ENTIDADE     = FIRU.COD_ENTIDADE      AND
                    RUE.COD_RUBRICA      = FIRU.COD_RUBRICA       AND
                    rue.cod_rubrica_sistema IS NOT NULL           AND 
                    LPAD(FINA.DAT_ANO,4,0)||LPAD(FINA.DAT_MES,2,0)= CDAD_PER_ULT_FICHA  
                              
             
             )
             LOOP
               IF W_Existe_calculo =0 THEN
 
                INSERT INTO TB_COMP_SAL_TRA 
                 (
                    --- Bloco de controle 
                        cod_ins
                       ,cod_ide_cli
                       ,num_matricula
                       ,cod_ide_rel_func
                       ,cod_entidade 
                       ,cod_adm_tra

                    ---- Bloco proincipal                   
                        ,cod_cargo_incorp
                        ,cod_cargo_2
                        ,cod_rubrica
                        ,cod_fcrubrica
                        ,cod_funcao
                        ,cod_ref_pad_venc
                        ,num_refe_calc
                        ,cod_tabela
                        ,dat_incorp
                        ,num_seq_vig
                        ,val_rubrica
                        ,val_unidade
                        ,val_por
                        ,val_prop_tram


                    ----- Bloco Secundario 
 
                    ----- Bloco de Auditoria 
                         ,dat_ing
                         ,dat_ult_atu
                         ,nom_usu_ult_atu
                         ,nom_pro_ult_atu

                  )
                  VALUES
                  (
                     --- Bloco de controle 
                        FICHA_FINANCEIRA_ATIVO.cod_ins
                       ,FICHA_FINANCEIRA_ATIVO.cod_ide_cli_serv
                       ,FICHA_FINANCEIRA_ATIVO.num_matricula
                       ,FICHA_FINANCEIRA_ATIVO.cod_ide_rel_func
                       ,FICHA_FINANCEIRA_ATIVO.cod_entidade 
                       ,P_COD_ADM_TRA 
                    ---- Bloco proincipal                   
                        ,FICHA_FINANCEIRA_ATIVO.cod_cargo
                        ,FICHA_FINANCEIRA_ATIVO.cod_cargo_2
                        ,FICHA_FINANCEIRA_ATIVO.cod_fcrubrica
                        ,FICHA_FINANCEIRA_ATIVO.cod_fcrubrica
                        ,FICHA_FINANCEIRA_ATIVO.cod_funcao
                        ,FICHA_FINANCEIRA_ATIVO.cod_referencia
                        ,FICHA_FINANCEIRA_ATIVO.cod_referencia_2
                        ,FICHA_FINANCEIRA_ATIVO.cod_tabela
                        ,FICHA_FINANCEIRA_ATIVO.dat_incorp
                        ,FICHA_FINANCEIRA_ATIVO.seq_vig
                        ,nvl(FICHA_FINANCEIRA_ATIVO.Val_fixo,0)
                        ,FICHA_FINANCEIRA_ATIVO.val_inidade
                        ,FICHA_FINANCEIRA_ATIVO.val_porc
                        ,FICHA_FINANCEIRA_ATIVO.val_porc2


                    ----- Bloco Secundario 
 
                    ----- Bloco de Auditoria                  
                         , sysdate -- dat_ing
                         , sysdate --dat_ult_atu
                         , PAR_COD_USER  --nom_usu_ult_atu
                         ,'Calculo de pesao' --nom_pro_ult_atu                 
                  );
                END IF;
               END LOOP;
   
              ---- Atualiza % da aposentadoria legado
              UPDATE TB_NPM_SERVIDOR NPM_SER
                     SET NPM_SER.PERC_BENEFICIO_FALECEU_ATIVIDADE =100
                     WHERE NPM_SER.COD_INS      =   P_COD_INS      AND
                           NPM_SER.COD_ADM_TRA  =   P_COD_ADM_TRA  AND
                           NPM_SER.COD_REGISTRO =   PAR_COD_REGISTRO ;
     
 
    EXCEPTION
        WHEN OTHERS THEN
                 OERRORCODE       :=90;
                 OERRORMESSAGE    :='Erro ao incluir Dados da ficha financeira de ativos';--'Erro ao obter dados da aposentadoaria origem';

  END;
 
 END;
 
 Procedure SP_OBTEM_DADOS_APOSENTADORIA_REGRA3
                                     (
                                      P_COD_INS             IN  NUMBER  ,
                                      P_COD_ADM_TRA         IN  VARCHAR ,
                                      P_DAT_CALC            IN  DATE    ,
                                      P_COD_IDE_CLI_SERV    IN  VARCHAR2,
                                      P_NUM_MATRICULA       IN  NUMBER  ,
                                      P_COD_IDE_REL_FUNC    IN  NUMBER  ,
                                      P_COD_ENTIDADE        IN  NUMBER  ,
                                      P_QTD_POR_SALARIOS    IN  NUMBER  ,
                                      P_TIP_CALCULO         IN  VARCHAR ,
                                      P_POR_CALCULO         IN  NUMBER  ,
                                      P_DAT_ING_SERV_PUB    IN  DATE    ,
                                      P_COD_TAREFA          IN  NUMBER  ,
                                      P_COD_REGISTRO        IN  NUMBER  ,
                                      P_COD_USER            IN VARCHAR  ,
                                      P_SEQBEN              IN NUMBER   ,
                                      OERRORCODE            OUT NUMBER  ,
                                      OERRORMESSAGE         OUT VARCHAR
                                      ) AS
 CDAD_PER_ULT_FICHA      VARCHAR2 (6);
 MESANOFATORCORRECAO     VARCHAR2 (6);
 NUMREGDE94ATEDTSOL      NUMBER   (5);
 NUMMESESDE94ATEDTSOL    NUMBER   (5);
 COD_CONTROLE            NUMBER(2):=90;
 NUMREGHISTCONTTUDO      NUMBER   (5);
 NUMREGHISTCONT          NUMBER   (5);
 PERMESESTRAB            NUMBER   (5);
 SALMEDIA_CAL            NUMBER   (18,4);
 TOTAL_SALARIOS          NUMBER   (18,4);
 DAT_INGRESS_VINCULO     DATE;
 BOL_GERA_REDUTOR        BOOLEAN  :=FALSE;
 VAL_TETO_RGPS           NUMBER   (18,4):=0;
 SERV_RUBRICA_LEI1345    NUMBER; 
 SERV_FLG_TETO_PREV_COMP CHAR(1);
 
 V_ERRO               EXCEPTION;
  
 BEGIN
   
      ----- Identifica periodo Carregado da tabela de reajuste.
      Dat_ingress_vinculo:=P_DAT_ING_SERV_PUB;
     BEGIN      
     SELECT  SUBSTR(MAX(SUBSTR(COD.COD_CUEN01, 3 , 4) || SUBSTR(COD.COD_CUEN01, 0 , 2)), 5,2) ||  SUBSTR(MAX(SUBSTR(COD.COD_CUEN01, 3 , 4) || SUBSTR(COD.COD_CUEN01, 0 , 2)), 0, 4)
              INTO    MesAnoFatorCorrecao
              FROM    PARCOD COD
              WHERE   COD.COD_CODNUM = 2355
              AND     COD.COD_CODINS = 0
              AND     SUBSTR(COD.COD_CUEN01, 3 , 4)  || SUBSTR(COD.COD_CUEN01, 0 , 2) <= TO_CHAR( P_DAT_CALC , 'RRRRMM')
              ORDER BY SUBSTR(COD.COD_CUEN01, 3 , 4) || SUBSTR(COD.COD_CUEN01, 0 , 2) DESC;
 
  
          Cod_controle         :=91;
     -- Obtem o numero de registros na tabela de fator de correcao
          SELECT COUNT(*) INTO numRegDe94AteDtSol FROM PARCOD COD
          WHERE COD.COD_CODNUM = 2355
            AND COD.COD_CODINS = 0
            AND COD.COD_NUME02 || TRIM(TO_CHAR(COD.COD_NUME01, '00')) <= TO_CHAR( P_DAT_CALC , 'RRRRMM') ---anoDtSolicitacao || mesDtSolicitacao
            AND COD.COD_CUEN01 = MesAnoFatorCorrecao ;

   
          NumMesesDe94AteDtSol:=MONTHS_BETWEEN(P_DAT_CALC, TO_DATE('01/07/1994','DD/MM/YYYY'));     

         IF numRegDe94AteDtSol + 3 < NumMesesDe94AteDtSol THEN
             Null;--  RAISE  V_ERRO;
          END IF;
 
          
                -- Obtem a quantidade de registro no historico de contribuicao
                -- para a relacao funcional em que o servidor solicitou o tramite
                -- a partir da data de 07/1994.

                    SELECT SUM(REM) 
                    INTO numRegHistContTudo 
                    FROM (
                              SELECT count(*) as REM
                              FROM VW_FICHA_FINANCEIRA DHC                
                              WHERE DHC.COD_INS             = P_COD_INS 
                                    AND DHC.COD_IDE_CLI     = P_COD_IDE_CLI_SERV
                                    AND DHC.NUM_MATRICULA   = P_NUM_MATRICULA
                                    AND DHC.COD_IDE_REL_FUNC= P_COD_IDE_REL_FUNC
                                    AND DHC.COD_TIP_FOLHA   = 1
                                    AND DHC.COD_ENTIDADE    = P_COD_ENTIDADE

                                    AND (DHC.DAT_MES NOT IN (1,2,3,4,5,6) AND DHC.DAT_ANO = 1994)
                                    AND DHC.DAT_ANO                           >= 1994
                                    AND DHC.DAT_ANO||LPAD(DHC.DAT_MES,2,'0')  <= TO_CHAR(P_DAT_CALC,'RRRRMM')
 
                              UNION ALL
                              SELECT count(*) as REM
                                FROM VW_FICHA_FINANCEIRA DHC
                                WHERE     DHC.COD_INS         = P_COD_INS 
                                      AND DHC.COD_IDE_CLI     = P_COD_IDE_CLI_SERV
                                      AND DHC.NUM_MATRICULA   = P_NUM_MATRICULA
                                      AND DHC.COD_IDE_REL_FUNC= P_COD_IDE_REL_FUNC
                                      AND DHC.COD_TIP_FOLHA   = 1
                                      AND DHC.COD_ENTIDADE    = P_COD_ENTIDADE
                                      AND DHC.DAT_ANO          >= 1995
                                      AND DHC.DAT_ANO||LPAD(DHC.DAT_MES,2,'0')  <= TO_CHAR(P_DAT_CALC,'RRRRMM')                   
 
                            
                    );
 
 
           --Buscando a quantidade de registros, considerando os salarios de remuneracao somente maiores do que zero.
                    SELECT SUM(REM) 
                     INTO NumRegHistCont 
                     FROM (
                              SELECT count(*) as REM   
 
                              FROM VW_FICHA_FINANCEIRA DHC                
                               WHERE DHC.COD_INS             = P_COD_INS 
                                    AND DHC.COD_IDE_CLI     = P_COD_IDE_CLI_SERV
                                    AND DHC.NUM_MATRICULA   = P_NUM_MATRICULA
                                    AND DHC.COD_IDE_REL_FUNC= P_COD_IDE_REL_FUNC
                                    AND DHC.COD_TIP_FOLHA   = 1
                                    AND DHC.COD_ENTIDADE    = P_COD_ENTIDADE
                                    AND (DHC.DAT_MES       NOT IN (1,2,3,4,5,6) AND DHC.DAT_ANO = 1994)
                                    AND DHC.DAT_ANO         >= 1994
                                    AND DHC.DAT_ANO||LPAD(DHC.DAT_MES,2,'0')  <= TO_CHAR(P_DAT_CALC,'RRRRMM')
                                    AND DHC.VAL_SAL_CON      > 0
                                    AND DHC.COD_ENTIDADE     = P_COD_ENTIDADE
                                    
                              UNION ALL
                              SELECT count(*) as REM 
 
                              FROM VW_FICHA_FINANCEIRA DHC                
                              WHERE DHC.COD_INS             = P_COD_INS 
                                    AND DHC.COD_IDE_CLI     = P_COD_IDE_CLI_SERV
                                    AND DHC.NUM_MATRICULA   = P_NUM_MATRICULA
                                    AND DHC.COD_IDE_REL_FUNC= P_COD_IDE_REL_FUNC
                                    AND DHC.COD_TIP_FOLHA   = 1
                                    AND DHC.COD_ENTIDADE    = P_COD_ENTIDADE
                                    AND DHC.DAT_ANO        >= 1995
                                    AND DHC.DAT_ANO||LPAD(DHC.DAT_MES,2,'0')  <= TO_CHAR(P_DAT_CALC,'RRRRMM')                     
                                    AND DHC.COD_TIP_FOLHA  = 1                     
                                    AND DHC.VAL_SAL_CON    > 0
                                    AND DHC.COD_ENTIDADE   = P_COD_ENTIDADE
                    );
 
                PerMesesTrab := FLOOR(NumRegHistCont * P_QTD_POR_SALARIOS/100);
 
 
 
 
                     -- Considerando o numero de registros independente se remunercao igual ou diferente de zero.
 
                IF ( 
                         (PerMesesTrab  > 0) 
                       
                  ) THEN
                            BEGIN
                                  
                                
                                  SELECT REM/perMesesTrab , REM
                                  INTO Salmedia_cal,  Total_salarios
                                  FROM (
                                    SELECT SUM(REM) AS REM
                                    FROM (
                                                --SELECT DHC.VAL_SAL_REM *COD.COD_PORCEN AS REM
                                              SELECT DHC.VAL_SAL_CON *COD.COD_PORCEN AS REM
                                               FROM VW_FICHA_FINANCEIRA DHC, PARCOD COD
                                           WHERE   DHC.COD_INS          = P_COD_INS 
                                                AND DHC.COD_IDE_CLI     = P_COD_IDE_CLI_SERV
                                                AND DHC.NUM_MATRICULA   = P_NUM_MATRICULA
                                                AND DHC.COD_IDE_REL_FUNC= P_COD_IDE_REL_FUNC
                                                AND DHC.COD_TIP_FOLHA   = 1
                                                AND DHC.COD_ENTIDADE    = P_COD_ENTIDADE
                                                AND DHC.DAT_ANO         = COD.COD_NUME02
                                                AND DHC.DAT_MES         = COD.COD_NUME01
                                                AND COD.COD_CODNUM      = 2355
                                                AND COD.COD_CODINS      = 0
                                                AND COD.COD_CUEN01      = mesAnoFatorCorrecao
                                                 --Nao estaremos mais considerando o valor do piso.
                                                --AND DHC.VAL_SAL_REM*COD.COD_PORCEN > valorPiso
                                                
                                                 --Considerando ass fichas financeiras do tipo NORMAL somente, assim evitamos duplicidades.
                                                AND DHC.COD_TIP_FOLHA = 1 
                                                 --Como solicitado, buscando somente os registros onde o valor do salario de remuneracao e maior do que zero.
 
                                                AND DHC.VAL_SAL_CON > 0
                                                
                                                 --Inserindo a entidade no SQL.
                                               --Nao estaremos mais considerando o valor do piso.
                                                
                                              ORDER BY REM DESC
                                      
                                      )
                                      
                                    WHERE ROWNUM < perMesesTrab+1
                                    
                                    );

                                  -- Insere na tabela de media aritmetica, mas antes limpa a tabela
           
                                     BEGIN    
                                     
                                          INSERT INTO  TB_HMEDIA_ARITMETICA_NL a
                                          (
                                            cod_ins                           , 
                                            cod_adm_tra                       ,
                                            cod_tarefa                        ,
                                            cod_registro                      ,
                                            cod_entidade                      ,
                                            cod_ide_cli_serv                  ,
                                            num_matricula                     ,
                                            cod_ide_rel_func                  ,
                                            cod_tipo_media                    ,
                                            mto_val_media                     ,
                                            por_val_media                     ,
                                            dat_ing                           ,
                                            dat_ult_atu                       ,
                                            nom_usu_ult_atu                   ,
                                            nom_pro_ult_atu                   ,
                                            qtd_meses_media                   ,
                                            val_total_salarios                ,
                                            mto_val_media_bruta               ,
                                            num_pap_seq                       ,
                                            num_pap_num_cor_pro  
                                          )               
                                           VALUES
                                          (
                                            P_COD_INS            , 
                                            P_COD_ADM_TRA        ,
                                            P_COD_TAREFA         ,
                                            P_COD_REGISTRO       ,
                                            P_COD_ENTIDADE       ,
                                            P_COD_IDE_CLI_SERV   ,
                                            P_NUM_MATRICULA      ,
                                            P_COD_IDE_REL_FUNC   ,
                                            '04'                 ,
                                            round(trunc(Salmedia_cal * (P_POR_CALCULO/100),4),2), 
                                            P_POR_CALCULO        ,  
                                            sysdate              ,
                                            sysdate              ,
                                            P_COD_USER           ,
                                            'Calculo Media'      ,
                                             perMesesTrab        ,
                                             round(trunc(Total_salarios,4),2) ,
                                             round(trunc(Salmedia_cal  ,4),2) ,
                                             P_SEQBEN ,
                                             0             
                                          ) ;            
                                      
                                     END;
   
                                 INSERT INTO TB_MEDIA_ARITMETICA_NL
                                   (
                                              cod_ins             , 
                                              cod_adm_tra         ,
                                              cod_tarefa          ,
                                              cod_registro        ,
                                              cod_entidade        ,
                                              cod_ide_cli_serv    ,
                                              num_matricula       ,
                                              cod_ide_rel_func    ,
                                              cod_tipo_media      ,
                                              num_seq             ,
                                              ----------------    
                                              rem_atual           ,
                                              rem_corrigida       ,
                                              ano_mes             ,
                                              ano_salario         ,
                                              mes_salario         ,
                                              flg_por             ,
                                              fat_correcao        ,
                                              ------------------
                                              dat_ing             ,
                                              dat_ult_atu         ,
                                              nom_usu_ult_atu     ,
                                              nom_pro_ult_atu     ,
                                              num_pap_seq         ,
                                              num_pap_num_cor_pro 
                                    )
                                   SELECT
                                              P_COD_INS            , 
                                              P_COD_ADM_TRA        ,
                                              P_COD_TAREFA         ,
                                              P_COD_REGISTRO       ,
                                              P_COD_ENTIDADE       ,
                                              P_COD_IDE_CLI_SERV   ,
                                              P_NUM_MATRICULA      ,
                                              P_COD_IDE_REL_FUNC   ,
                                              '04'                 ,
                                              rownum               ,
                                              --------------------
                                              tb2.sal             ,            
                                              tb2.rem             ,           
                                              tb2.dat             ,
                                              tb2.ano_salario     ,
                                              tb2.mes_salario     ,             
 
                                              DECODE(tb1.dat, NULL, 'N', 'S') , --- FLG_POR 
                                              TB2.COD_PORCEN       ,            --- FAT_CORRECAO              
                                              --------------------  
                                              sysdate              ,
                                              sysdate              ,
                                              P_COD_USER           ,
                                              'Calculo Media'      ,                                          
                                                ------------       ,
                                              P_SEQBEN ,
                                              0
           
                                   FROM (
                                      SELECT * 
                                      FROM (
                                             SELECT 
                                                   DHC.VAL_SAL_CON AS SAL,
                                                   DHC.VAL_SAL_CON*COD.COD_PORCEN AS REM,
                                                   DHC.DAT_ANO || TRIM(TO_CHAR(DHC.DAT_MES, '00')) AS DAT,
                                                   DHC.DAT_ANO                      ANO_SALARIO,
                                                   TRIM(TO_CHAR(DHC.DAT_MES, '00')) MES_SALARIO,
                                                   COD.COD_PORCEN
                                              FROM VW_FICHA_FINANCEIRA DHC, PARCOD COD
                                          WHERE   DHC.COD_INS             = P_COD_INS 
                                              AND DHC.COD_IDE_CLI     = P_COD_IDE_CLI_SERV
                                              AND DHC.NUM_MATRICULA   = P_NUM_MATRICULA
                                              AND DHC.COD_IDE_REL_FUNC= P_COD_IDE_REL_FUNC
                                              AND DHC.COD_TIP_FOLHA   = 1
                                              AND DHC.COD_ENTIDADE    = P_COD_ENTIDADE
                                              AND DHC.DAT_ANO         = COD.COD_NUME02
                                              AND DHC.DAT_MES         = COD.COD_NUME01
                                              AND COD.COD_CODNUM      = 2355
                                              AND COD.COD_CODINS      = 0
                                              AND COD.COD_CUEN01      = mesAnoFatorCorrecao
                                              
                                              --Modificacao: Frederico Oliveira - Data: 22/09/2010.
                                              --Nao estaremos mais considerando o valor do piso.
                                              --AND DHC.VAL_SAL_REM *COD.COD_PORCEN > valorPiso
                                              
                                              --Considerando ass fichas financeiras do tipo NORMAL somente, assim evitamos duplicidades.
                                              AND DHC.COD_TIP_FOLHA = 1   
                                              
                                               --Como solicitado, buscando somente os registros onde o valor do salario de remuneracao e maior do que zero.
                                               --AND DHC.VAL_SAL_REM > 0
                                              AND DHC.VAL_SAL_CON > 0
                                               --Nao estaremos mais considerando o valor do piso.
                                              
                                            ORDER BY REM DESC
                                      )
                                      
                                      
                                  WHERE ROWNUM < perMesesTrab+1) tb1,
                                       (
                                      
                                          SELECT DHC.VAL_SAL_CON AS SAL,
                                                 DHC.VAL_SAL_CON*COD.COD_PORCEN AS REM,
                                                 DHC.DAT_ANO || TRIM(TO_CHAR(DHC.DAT_MES, '00')) AS DAT,
                                                 DHC.DAT_ANO                      ANO_SALARIO,
                                                 TRIM(TO_CHAR(DHC.DAT_MES, '00')) MES_SALARIO,                                                 
                                                 COD.COD_PORCEN                    
                                            FROM VW_FICHA_FINANCEIRA DHC, PARCOD COD
                                         WHERE   DHC.COD_INS             = P_COD_INS 
                                              AND DHC.COD_IDE_CLI        = P_COD_IDE_CLI_SERV
                                              AND DHC.NUM_MATRICULA      = P_NUM_MATRICULA
                                              AND DHC.COD_IDE_REL_FUNC   = P_COD_IDE_REL_FUNC
                                              AND DHC.COD_TIP_FOLHA      = 1
                                              AND DHC.COD_ENTIDADE       = P_COD_ENTIDADE
                                              AND DHC.DAT_ANO            = COD.COD_NUME02
                                              AND DHC.DAT_MES            = COD.COD_NUME01
                                              AND COD.COD_CODNUM         = 2355
                                              AND COD.COD_CODINS         = 0
                                              AND COD.COD_CUEN01         = mesAnoFatorCorrecao
                                            
                                             --Nao estaremos mais considerando o valor do piso.
                                            --AND DHC.VAL_SAL_REM*COD.COD_PORCEN > valorPiso
                                            
                                            --Modificacao: Frederico Oliveira - Data: 14/09/2010
                                            --Considerando ass fichas financeiras do tipo NORMAL somente, assim evitamos duplicidades.
                                           
                                      ) TB2

                                      WHERE TB1.DAT (+) = TB2.DAT
                                      ORDER BY  DAT;

                            END; --- Fim quando Qtd de meses  (per80MesesTrab > 0) 
                    ELSE --- Qtd de meses  (per80MesesTrab <= 0)
                           BEGIN
                               NULL;
                              -- Cod_controle:=93;
                              --  RAISE V_ERRO;
                           END;
                    END IF;


               ----- Bloque de Gera??o de Tabela de Pagamento do Fluxo -----
               ---- Vamos gerar a rubrica de principal sem desconto do  redutor
               ----   e rubrica de redutor se corresponde   
                  IF Salmedia_cal  >0 THEN 
 
                                IF  SP_VALIDA_TETO_COMPLEMETAR (P_COD_ENTIDADE,P_DAT_ING_SERV_PUB) THEN
                                    SERV_FLG_TETO_PREV_COMP  :='S';
                                ELSE
                                     SERV_FLG_TETO_PREV_COMP :='N';
                                END IF;
                                SERV_RUBRICA_LEI1345 := SP_OBTEM_RUBRICA_LEI1354(P_COD_INS,SERV_FLG_TETO_PREV_COMP );
 
                                  INSERT INTO TB_COMP_SAL_TRA 
                                     (
                                        --- Bloco de controle 
                                           cod_ins
                                          ,cod_ide_cli
                                          ,num_matricula
                                          ,cod_ide_rel_func
                                          ,cod_entidade 
                                          ,cod_adm_tra

                                        ---- Bloco proincipal                   
                                            ,cod_cargo_incorp
                                            ,cod_cargo_2
                                            ,cod_rubrica
                                            ,cod_fcrubrica
                                            ,cod_funcao
                                            ,cod_ref_pad_venc
                                            ,num_refe_calc
                                            ,cod_tabela
                                            ,dat_incorp
                                            ,num_seq_vig
                                            ,val_rubrica
                                            ,val_unidade
                                            ,val_por
                                            ,val_prop_tram 


                     
                                        ----- Bloco de Auditoria 
                                             ,dat_ing
                                             ,dat_ult_atu
                                             ,nom_usu_ult_atu
                                             ,nom_pro_ult_atu

                                      )
                                      VALUES
                                      (
                                         --- Bloco de controle 
                                            P_COD_INS 
                                           ,P_COD_IDE_CLI_SERV
                                           ,P_NUM_MATRICULA
                                           ,P_COD_IDE_REL_FUNC
                                           ,P_COD_ENTIDADE 
                                           ,P_COD_ADM_TRA
                                        ---- Bloco proincipal                   
                                            ,null
                                            ,null
                                            ,SERV_RUBRICA_LEI1345
                                            ,SERV_RUBRICA_LEI1345
                                            ,null
                                            ,null
                                            ,null
                                            ,null
                                            ,null
                                            ,1
                                            ,round(trunc(Salmedia_cal * (P_POR_CALCULO/100),4),2)
                                            ,null
                                            ,null
                                            ,null


                                        ----- Bloco Secundario 
                             
                                        ----- Bloco de Auditoria                  
                                             , sysdate -- dat_ing
                                             , sysdate --dat_ult_atu
                                             , P_COD_USER  --nom_usu_ult_atu
                                             ,'Calculo de pesao' --nom_pro_ult_atu                 
                                      );
              
                  END IF;
        ----- Executa dados de Rubricas ------
 
 
       PAC_CALCULO_VALOR_RUBRICAS.PAC_CALCULO_RUBRICAS_NL(
                P_COD_INS          , 
                TO_DATE('01/'||TO_CHAR(P_DAT_CALC,'MM/YYYY'),'DD/MM/YYYY'),--PERIODOFOLHA,
                PAR_COD_USER       ,
                P_COD_ADM_TRA      ,
                P_COD_IDE_CLI_SERV ,
                'M'                ,  
                P_DAT_CALC         , 
                DAT_INGRESS_VINCULO, 
                OERRORMESSAGE); 

     
     
      
      EXCEPTION

        WHEN  V_ERRO THEN   
                OERRORCODE       := 92;
                OERRORMESSAGE   :='Erro ao identifica periodo Carregado da tabela de reajuste';
                   
        WHEN OTHERS THEN
                 OERRORCODE       := Cod_controle;
                 OERRORMESSAGE    :=SQLERRM;-- 'Erro ao identifica periodo Carregado da tabela de reajuste';
  
     END;
 
 END;
  PROCEDURE SP_REGISTRA_DADOS_PENSAO
  (
         P_COD_INS      IN NUMBER       ,
         P_COD_ADM_TRA  IN VARCHAR2     ,
         P_COD_REGISTRO IN NUMBER       ,
         P_COD_USER     IN VARCHAR      ,
         P_COD_ERRO    OUT NUMBER      ,
         P_MSG_ERRO    OUT VARCHAR2
  ) AS
  
 /****** variaveis de Erro ****/
  v_erro_tipo99             exception;
  v_erro_tipo10            exception;
  v_erro_tipo11            exception;
  v_erro_tipo12            exception;
  v_erro_tipo13            exception;
  v_erro_tipo14            exception;
  v_erro_tipo15            exception;
 
 ---- Varioaves de apoio 
  RGDAD_IND_TIP_CALC       number;
  MAX_NUM_SEQ              number;
  
  V_BOOL BOOLEAN := FALSE;
  
  v_qtd  number;
  v_cod_registro number;
 
  BEGIN  
  
    
    BEGIN  
     
      /*     SELECT    COUNT(*)  
            INTO QTD_CONTROLE_TAREFA 
             FROM WRKREG P
            WHERE P.REG_COD_INS     =    P_COD_INS     AND 
                  P.REG_COD_TIPO    =   'NPM'          AND
                  P.REG_COD_REGISTRO=   P_COD_REGISTRO AND
                  P.REG_COD_ESTADO  =   'T';
                  
         IF  QTD_CONTROLE_TAREFA  <> 0 THEN 
             RETURN;
         END IF;
               */
               
               
                
           select count(1)
              into v_qtd
             from wrkcli cli
            where cli.cli_cod_ins = p_cod_ins
              and cli.cli_cod_adm_tra = P_COD_ADM_TRA
              and cli.cli_ind_tip_tra = 'NPI';
              
            if v_qtd > 0 then 
              v_cod_registro := fnc_npm_ret_ult_cod_registro(i_cod_ins      => P_COD_INS,
                                                             i_cod_adm_tra  => P_COD_ADM_TRA,
                                                             i_cod_registro => P_COD_REGISTRO)   ;
              select count(1)
                into v_qtd
                from tb_comp_sal_tra tra
               where tra.cod_ins = p_cod_ins
                 and tra.cod_adm_tra = P_COD_ADM_TRA;
                 
              if  v_qtd = 0 then   
                SP_ADM_DADOS_PENSAO(P_COD_INS      => P_COD_INS,
                                    P_COD_ADM_TRA  => P_COD_ADM_TRA,
                                    P_COD_REGISTRO => v_cod_registro,
                                    P_COD_USER     => P_COD_USER,
                                    P_COD_ERRO     => P_COD_ERRO,
                                    P_MSG_ERRO     => P_MSG_ERRO);
               end if;                                    
            end if;                                  
                                            
                     --- Obtem sequencia a atualizar 
            SELECT MAX(PSER.NUM_SEQ) 
                           INTO   MAX_NUM_SEQ 
                           FROM TB_NPM_SERVIDOR PSER
                           WHERE PSER.COD_INS      =   P_COD_INS        AND
                                 PSER.COD_ADM_TRA  =   P_COD_ADM_TRA    AND
                                 PSER.COD_REGISTRO =   P_COD_REGISTRO ;
               
              IF NVL(MAX_NUM_SEQ  ,0) > 0 THEN
                         SELECT
                               S.TRP_SEQ           ,
                               S.TRP_FEC_SOL_TRA   ,
                               S.TRP_IDE_CLI       ,
                               S.TRP_IND_TIP_TRA   ,
                               S.TRP_COD_ADM_TRA   ,
                               S.TRP_IDE_PER       ,
                               S.TRP_NUM_MAT       ,
                               S.TRP_COD_TRA_COMPL ,
                               S.TRP_IDE_REL_FUNC  ,
                               S.TRP_COD_ENTIDADE  ,
                               S.TRP_CARGO_PEN     ,
                               SER.COD_TAREFA      ,
                               SER.DAT_ING_SERV_PUB,
                               FI.DAT_OBITO    
                          INTO PAR_TRP_SEP         , 
                               PAR_DT_SOLICITACAO  ,
                               PAR_COD_IDE_CLI     ,
                               PAR_TIP_BEN         ,
                               PAR_COD_ADM_TRA     ,
                               PAR_IDE_PER         ,
                               PAR_NUM_MAT         ,
                               PAR_COD_TRA_COMPL   ,
                               PAR_IDE_REL_FUNC    ,
                               PAR_COD_ENTIDADE    ,
                               PAR_COD_CARGO       ,
                               PAR_COD_TAREFA      ,
                               PAR_DAT_ING_SERV_PUB,
                               PAR_DATA_OBITO              
                        FROM SIMTRP S, TB_NPM_SERVIDOR SER, TB_PESSOA_FISICA FI
                        WHERE S.TRP_COD_ADM_TRA  = P_COD_ADM_TRA
                          AND S.TRP_COD_INS      = P_COD_INS
                          AND SER.COD_INS        = P_COD_INS
                          AND SER.COD_ADM_TRA    = P_COD_ADM_TRA
                          AND SER.COD_REGISTRO   = P_COD_REGISTRO
                          AND SER.NUM_SEQ        = MAX_NUM_SEQ
                          AND FI.COD_INS         = P_COD_INS
                          AND FI.COD_IDE_CLI     = SER.COD_IDE_CLI_SERV;
               ELSE
                    RAISE  v_erro_tipo11;
               END IF;

                EXCEPTION
                 When OTHERS Then
                  RAISE  v_erro_tipo11;
                END;
                
               ----- Obtem Regra de Calculo ------
               BEGIN
                SELECT  
                  PAP.PAP_IND_TIP_CALC 
                  INTO RGDAD_IND_TIP_CALC 
                FROM SIMPAP PAP
                WHERE PAP.PAP_COD_INS      = P_COD_INS       AND
                      PAP.PAP_COD_ADM_TRA  = P_COD_ADM_TRA   AND
                      PAP.PAP_IDE_CLI      = PAR_COD_IDE_CLI; 
                EXCEPTION
                 When no_data_found then              
                      RAISE  v_erro_tipo11;
                                       
                 When OTHERS Then
                  RAISE  v_erro_tipo11;
                END;          
                SP_REGISTRA_DADOS_UPD (P_COD_INS          ,
                                       PAR_COD_ADM_TRA     ,
                                       RGDAD_IND_TIP_CALC  ,
                                       PAR_COD_USER        , 
                                       PAR_COD_ENTIDADE    , 
                                       PAR_DAT_ING_SERV_PUB,
                                       PAR_DATA_OBITO      , 
                                       MAX_NUM_SEQ         ,
                                       PAR_COD_ADM_TRA     ,
                                       oErrorCode          ,
                                       oErrorMessage);
           
                                       IF oErrorCode <>  0 THEN
                                          raise  v_erro_tipo14;
                                        END IF;
              
            
              IF    PAR_COD_TRA_COMPL IS NOT NULL THEN
                       BEGIN
           
                      SELECT MAX(PSER.NUM_SEQ) 
                               INTO   MAX_NUM_SEQ 
                               FROM TB_NPM_SERVIDOR PSER
                               WHERE PSER.COD_INS      =   P_COD_INS         AND
                                     PSER.COD_ADM_TRA  =   PAR_COD_TRA_COMPL AND
                                     PSER.COD_REGISTRO =   P_COD_REGISTRO ;
           
                       SELECT
                             S.TRP_SEQ          ,
                             S.TRP_FEC_SOL_TRA  ,
                             S.TRP_IDE_CLI      ,
                             S.TRP_IND_TIP_TRA  ,
                             S.TRP_COD_ADM_TRA  ,
                             S.TRP_IDE_PER      ,
                             S.TRP_NUM_MAT      ,
                             S.TRP_COD_TRA_COMPL,
                             S.TRP_IDE_REL_FUNC ,
                             S.TRP_COD_ENTIDADE ,
                             S.TRP_CARGO_PEN    ,
                             SER.COD_TAREFA     ,
                             SER.DAT_ING_SERV_PUB,
                             FI.DAT_OBITO    
                        INTO PAR_TRP_SEP        ,
                             PAR_DT_SOLICITACAO ,
                             PAR_COD_IDE_CLI    ,
                             PAR_TIP_BEN        ,
                             PAR_COD_ADM_TRA    ,
                             PAR_IDE_PER        ,
                             PAR_NUM_MAT        ,
                             PAR_COD_TRA_COMPL2 ,
                             PAR_IDE_REL_FUNC   ,
                             PAR_COD_ENTIDADE   ,
                             PAR_COD_CARGO      ,
                             PAR_COD_TAREFA     ,
                             PAR_DAT_ING_SERV_PUB,
                             PAR_DATA_OBITO   
                             
                      FROM SIMTRP S, TB_NPM_SERVIDOR SER,TB_PESSOA_FISICA FI
                      WHERE S.TRP_COD_ADM_TRA  = PAR_COD_TRA_COMPL
                        AND S.TRP_COD_INS      = P_COD_INS
                        AND SER.COD_INS        = P_COD_INS
                        AND SER.COD_ADM_TRA    = PAR_COD_TRA_COMPL
                        AND SER.COD_REGISTRO   = P_COD_REGISTRO
                        AND SER.NUM_SEQ        = MAX_NUM_SEQ 
                        AND FI.COD_INS         = P_COD_INS
                        AND FI.COD_IDE_CLI     = SER.COD_IDE_CLI_SERV; 
              
                      EXCEPTION
                       When OTHERS Then
                        RAISE  v_erro_tipo11;
                      END;     
              
                SP_REGISTRA_DADOS_UPD (P_COD_INS         ,
                                       PAR_COD_TRA_COMPL   ,
                                       RGDAD_IND_TIP_CALC  ,
                                       PAR_COD_USER        ,
                                       PAR_COD_ENTIDADE    ,
                                       PAR_DAT_ING_SERV_PUB, 
                                       PAR_DATA_OBITO      ,                              
                                       MAX_NUM_SEQ         , 
                                       P_COD_ADM_TRA       ,
                                       oErrorCode          ,
                                       oErrorMessage);
           
              IF oErrorCode <>  0 THEN
                raise  v_erro_tipo15;
              END IF;


              END IF;
   --END IF;
  
    EXCEPTION
      WHEN v_erro_tipo10 THEN
        P_COD_ERRO    := 10;
        P_MSG_ERRO    :=  OERRORMESSAGE;

      WHEN v_erro_tipo11 THEN
        P_COD_ERRO    := 10;
        P_MSG_ERRO    := 'Nao existe registro de controle ';

      WHEN v_erro_tipo12 THEN
        P_COD_ERRO    := 10;
        P_MSG_ERRO    := 'Erro no calculo do Vinculo principal Existe beneficio de Pens?o'||'- ('|| PAR_COD_ADM_TRA||')'  ;


     WHEN v_erro_tipo13 THEN
        P_COD_ERRO    := 10;
        P_MSG_ERRO    := 'Erro aposentodoria origem n?o identificada'||'- ('|| PAR_COD_ADM_TRA||')'  ;


      WHEN v_erro_tipo14 THEN
        P_COD_ERRO    := 10;
        P_MSG_ERRO    := 'Valor do beneficio est? zerado '||'- ('|| PAR_COD_ADM_TRA||')'  ;

      WHEN v_erro_tipo15 THEN
        P_COD_ERRO    := 10;
        P_MSG_ERRO    := 'Valor do beneficio est? zerado '||'- ('|| PAR_COD_TRA_COMPL ||')'  ;


   WHEN v_erro_tipo99 THEN
          NULL;

      WHEN OTHERS THEN
        P_COD_ERRO    := 10;
        P_MSG_ERRO    := SQLERRM;

    
    
  END;
 
 PROCEDURE SP_REGISTRA_DADOS_UPD (P_COD_INS                IN NUMBER   ,
                                  P_COD_ADM_TRA            IN VARCHAR2 ,
                                  P_IND_TIP_CALC           IN NUMBER   ,
                                  P_COD_USER               IN VARCHAR2 ,
                                  P_COD_ENTIDADE           IN NUMBER   ,
                                  P_DAT_ING_SERV_PUB       IN DATE     , 
                                  P_DAT_CALC               IN DATE     ,
                                  P_MAX_NUM_SEQ            IN NUMBER   ,
                                  P_COD_ADM_TRA_PRINC      IN VARCHAR2 ,
                                  OERRORCODE       OUT NUMBER    ,
                                  OERRORMESSAGE    OUT VARCHAR )AS 

  SERV_VAL_TOTAL_BENEFICIO            NUMBER(20,6):=0;
  SERV_VFLG_TETO_PREV_COMPL           VARCHAR2(1) :=0;
  SERV_VVAL_TETO_RGPS_PREV            NUMBER(20,6):=0;
  SERV_VVAL_TOTAL_FINAL_PROP          NUMBER(20,6):=0;
  DAT_INGRESS_VINCULO                 DATE;
  SERV_FLG_TETO_PREV_COMP             CHAR(1);
  SERV_VAL_COTA_FAMILIAR              NUMBER(20,6):=0;
  SERV_RUBRICA_LEI1345                NUMBER;
 
 
  VALOR_TETO_RGPR                     NUMBER(20,6):=0;
  MAX_NUM_SEQ_BENEF                   NUMBER(20,6):=0;
  PBEN_FLG_INVALIDO_DEFICIENCIA       CHAR(1); 
  PBEN_EXISTE_INVALIDO                BOOLEAN:=FALSE;
  PBEN_CONT                           NUMBER(20,6):=0; 
  PBEN_CONT_NORMAL                       NUMBER(20,6):=0; 
  PBEN_CONT_NAO_EX                       NUMBER(20,6):=0; 
 
  PBEN_FLG_POSSUI_BEN_PREV               CHAR(1);
  PBEN_FLG_OPCAO_BENEFICIO_VANTAJOSO     CHAR(1);
  PBEN_PROPORCAO                         NUMBER(18,4):=0;
  PBEN_BENEFICIO_ACUM                    NUMBER(18,4):=0;   
  QTD_BENEFICIO_AUX                      NUMBER;
  VAL_BENEFICIO_AUX                      NUMBER(18,4):=0; 
  SERV_DATA_INICIO_BENEFICIO             DATE;
  DATA_INICIO_BENEFICIO                  DATE;
  VAL_RATEIO_BENEFICIO                   NUMBER(18,4):=0;
  VAL_RATEIO_BENEFICIO_NAO_EX            NUMBER(18,4):=0;
  PBEN_POR_BEN_PREV                      NUMBER(18,4):=0;
  PBEN_POR_TOT_BEN_PREV                  NUMBER(18,4):=0;
  PBEN_COD_PERFIL                        CHAR(5);
  PBEN_COD_PARENTESCO                    CHAR(5);  
  PBEN_FLG_DEFERIR                       CHAR(1);
  PBEN_DESC_EXIGENCIA_PERICIA_MEDICA     CHAR(1);
  PBEN_DESC_EXIGENCIA_ACUMULAT_CUMPRIDA  CHAR(1);
  PBEN_FLG_EXIGENCIA_DOC_CUMPRIDA        CHAR(1);
  PBEN_FLG_CAMINHO_EXIGENCIA_ACUMULATIVIDADE CHAR(1);
  PBEN_FLG_DEVE_CRIAR_EXIG_DOC               CHAR(1);
  PBEN_FLG_DEVE_CRIAR_EXIG_PER_MED           CHAR(1);
 
  
  BEGIN

    Dat_ingress_vinculo:=P_DAT_ING_SERV_PUB;    
    BEGIN  
        
    
  
     --- Obtem o valor do beneficio que soma as rubricas de credito do beneficio
     ---  Esta base ser? utilizada para gera a rubrica de desconto Evento Especial "e".
         SP_COMPOSICAO_BASE(
                           P_COD_INS               ,
                           2102000                 ,
                           'BASE_COMP'             ,
                           P_COD_ADM_TRA           ,
                           P_COD_ENTIDADE          ,
                           P_DAT_CALC              ,
                           SERV_VAL_TOTAL_BENEFICIO,
                           OERRORCODE              ,
                           OERRORMESSAGE           
                           ); 
  
        IF  NVL(SERV_VAL_TOTAL_BENEFICIO,0) = 0 THEN
                RAISE   v_erro ;
        END IF;
  
       -- Atualiza Valor de Deconto do Teto  ---
        SELECT NVL(SUM(DECODE(RUB.FLG_NATUREZA,'D',CTRA.VAL_RUBRICA,0)),0)
        INTO   SERV_VVAL_TETO_RGPS_PREV  
        FROM TB_COMP_SAL_TRA CTRA, TB_RUBRICAS RUB
        WHERE CTRA.COD_INS     = P_COD_INS           AND
              CTRA.COD_ADM_TRA = P_COD_ADM_TRA       AND
              RUB.COD_INS      = P_COD_INS           AND
              RUB.COD_ENTIDADE = CTRA.COD_ENTIDADE   AND
              RUB.COD_RUBRICA  = 2102000             AND
              RUB.COD_RUBRICA   =CTRA.COD_FCRUBRICA ;

        IF  SP_VALIDA_TETO_COMPLEMETAR (P_COD_ENTIDADE,P_DAT_ING_SERV_PUB) THEN
               SERV_FLG_TETO_PREV_COMP  :='S';
               VALOR_TETO_RGPR          :=SP_OBTEM_TETO_RGPS (P_DAT_CALC);
              IF SERV_VAL_TOTAL_BENEFICIO > VALOR_TETO_RGPR  THEN
                  SERV_VAL_TOTAL_BENEFICIO  := VALOR_TETO_RGPR ; 
                  SERV_VVAL_TETO_RGPS_PREV  := 0 ; 

              ELSE

                   SERV_VAL_TOTAL_BENEFICIO  :=ROUND(TRUNC(SERV_VAL_TOTAL_BENEFICIO,4),2);
                   SERV_VVAL_TETO_RGPS_PREV  := 0; 

              END IF;
          ELSE 
                   SERV_FLG_TETO_PREV_COMP :='N';
                   SERV_VAL_TOTAL_BENEFICIO  := ROUND(TRUNC(SERV_VAL_TOTAL_BENEFICIO,4),2);
                   SERV_VVAL_TETO_RGPS_PREV  := 0; 
        END IF;

       
       FOR BENEF_UPF IN (
          SELECT DISTINCT
                 PBENE.COD_ADM_TRA      ,
                 PBENE.COD_IDE_CLI_SERV ,
                 PBENE.COD_IDE_CLI_BEN
               FROM TB_NPM_BENEFICIARIO PBENE
               WHERE PBENE.COD_INS     = P_COD_INS     AND
                     PBENE.COD_ADM_TRA = P_COD_ADM_TRA
                     -------- Alterado em 27-07-2020 ---------
                     --- Deverea reservar cota do beneficiario
                     ---- Melhora da Planilha de melhoras 1
                     AND
                      ( PBENE.FLG_DEFERIR ='S' 
                      OR
                         (
                           PBENE.FLG_DEFERIR IS NULL AND
                           (
                                NVL(PBENE.DESC_EXIGENCIA_PERICIA_MEDICA     ,' ')   IN ('S','N')  OR
                                NVL(PBENE.DESC_EXIGENCIA_ACUMULAT_CUMPRIDA  ,' ')   IN ('S','N')  OR
                                NVL(PBENE.FLG_EXIGENCIA_DOC_CUMPRIDA        ,' ')   IN ('S','N')
                            
                          )
                        
                        )
                      )                    
                      
        )
        LOOP
            ----- Numero de Sequencis de Beneficiario   
            SELECT MAX(PBENE2.NUM_SEQ) 
                   INTO   MAX_NUM_SEQ_BENEF 
                   FROM TB_NPM_BENEFICIARIO PBENE2
                   WHERE PBENE2.COD_INS      =   P_COD_INS                   AND
                         PBENE2.COD_ADM_TRA  =   P_COD_ADM_TRA               AND
                         PBENE2.COD_IDE_CLI_SERV =BENEF_UPF.COD_IDE_CLI_SERV AND
                         PBENE2.COD_IDE_CLI_BEN  =BENEF_UPF.COD_IDE_CLI_BEN;         
  
            SELECT PBENE2.FLG_INVALIDO_DEFICIENCIA,
                   PBENE2.DAT_INI_BEN             ,
                   PBENE2.PROPORCAO               ,
                   BNIPA.IPA_COD_PERFIL           ,
                   BNIPA.IPA_COD_PARENTESCO       ,
                   PBENE2.FLG_DEFERIR             ,
                   PBENE2.DESC_EXIGENCIA_PERICIA_MEDICA    ,
                   PBENE2.DESC_EXIGENCIA_ACUMULAT_CUMPRIDA ,
                   PBENE2.FLG_EXIGENCIA_DOC_CUMPRIDA       ,
                         PBENE2.FLG_CAMINHO_EXIGENCIA_ACUMULATIVIDADE ,
                         PBENE2.FLG_DEVE_CRIAR_EXIG_DOC               ,
                         PBENE2.FLG_DEVE_CRIAR_EXIG_PER_MED                          
                   INTO  PBEN_FLG_INVALIDO_DEFICIENCIA,
                         DATA_INICIO_BENEFICIO     ,
                         PBEN_POR_BEN_PREV         ,
                         PBEN_COD_PERFIL           ,
                         PBEN_COD_PARENTESCO       ,
                         PBEN_FLG_DEFERIR          ,      
                         PBEN_DESC_EXIGENCIA_PERICIA_MEDICA    ,
                         PBEN_DESC_EXIGENCIA_ACUMULAT_CUMPRIDA ,
                         PBEN_FLG_EXIGENCIA_DOC_CUMPRIDA       ,                                                   
                         PBEN_FLG_CAMINHO_EXIGENCIA_ACUMULATIVIDADE ,
                         PBEN_FLG_DEVE_CRIAR_EXIG_DOC               ,
                         PBEN_FLG_DEVE_CRIAR_EXIG_PER_MED           
                          
                   FROM TB_NPM_BENEFICIARIO PBENE2,
                        BENIPA              BNIPA         
                   WHERE PBENE2.COD_INS      =   P_COD_INS                   AND
                         PBENE2.COD_ADM_TRA  =   P_COD_ADM_TRA               AND
                         PBENE2.COD_IDE_CLI_SERV =BENEF_UPF.COD_IDE_CLI_SERV AND
                         PBENE2.COD_IDE_CLI_BEN  =BENEF_UPF.COD_IDE_CLI_BEN  AND
                         PBENE2.NUM_SEQ          = MAX_NUM_SEQ_BENEF         AND
                         BNIPA.IPA_COD_INS       = P_COD_INS                 AND
                         BNIPA.IPA_IDE_CLI       = BENEF_UPF.COD_IDE_CLI_BEN AND
                         BNIPA.IPA_COD_ADM_TRA   =  P_COD_ADM_TRA_PRINC ; 
                         
                         
                         
             IF PBEN_FLG_INVALIDO_DEFICIENCIA= 'S' THEN  

                PBEN_EXISTE_INVALIDO:=TRUE;

             END IF;
               
             IF     PBEN_CONT < 5                  THEN
 
                    PBEN_CONT:=PBEN_CONT+1;
 
             END IF;
             
             IF   PBEN_FLG_DEFERIR   ='S'  
                -------- Alterado em 03-03-2021 ---------
                --- Considerar todos os tipos de Exigencia.
                OR (
                         nvl(PBEN_FLG_CAMINHO_EXIGENCIA_ACUMULATIVIDADE,'N') ='S'
                   OR    nvl(PBEN_FLG_DEVE_CRIAR_EXIG_DOC      ,'N')         ='S'
                   OR    nvl(PBEN_FLG_DEVE_CRIAR_EXIG_PER_MED  ,'N')         ='S'
                   ) 
             THEN
                   IF     SERV_DATA_INICIO_BENEFICIO    IS NULL 
                       OR  
                         DATA_INICIO_BENEFICIO >  SERV_DATA_INICIO_BENEFICIO  THEN  
                        SERV_DATA_INICIO_BENEFICIO  :=DATA_INICIO_BENEFICIO;
                      
                   END IF; 
                   
                   ------ Total de % de Rateio de Beneficiarios -----------------
         
                   
                    --PBEN_COD_PERFIL           ,
                    IF  PBEN_COD_PERFIL IN  (8,801,803,590,80,90,63)   THEN
                         VAL_RATEIO_BENEFICIO_NAO_EX  := PBEN_POR_BEN_PREV + VAL_RATEIO_BENEFICIO_NAO_EX;
                         PBEN_CONT_NAO_EX :=PBEN_CONT_NAO_EX +1;
                    ELSE
                          VAL_RATEIO_BENEFICIO         := PBEN_POR_BEN_PREV +VAL_RATEIO_BENEFICIO;
                          PBEN_CONT_NORMAL :=PBEN_CONT_NORMAL +1;  
                    END IF;  
            END IF; 
             
        END LOOP;
         IF PBEN_CONT_NORMAL > 0 THEN
            PBEN_POR_TOT_BEN_PREV :=ROUND(((100 -VAL_RATEIO_BENEFICIO_NAO_EX))/(PBEN_CONT-PBEN_CONT_NAO_EX) ,4) ;
         ELSE
             PBEN_POR_TOT_BEN_PREV :=0 ;
         END IF;
          ---------------- Regra de Atualiza??o de beneficios  ----------------
         VALOR_TETO_RGPR:= SP_OBTEM_VARIAVEL ( 
                              'TASCO'     , 
                              2000        , 
                              'DESC_CONTR',
                              P_DAT_CALC 
                              );
         
         ---- Calculo da Contas Familiar  ----
          IF  PBEN_EXISTE_INVALIDO  THEN
              VALOR_TETO_RGPR          :=SP_OBTEM_TETO_RGPS (P_DAT_CALC);
              IF SERV_VAL_TOTAL_BENEFICIO > VALOR_TETO_RGPR AND VALOR_TETO_RGPR > 0  THEN
                  SERV_VAL_COTA_FAMILIAR := SERV_VAL_TOTAL_BENEFICIO -VALOR_TETO_RGPR;
                  SERV_VAL_COTA_FAMILIAR := VALOR_TETO_RGPR   +(SERV_VAL_COTA_FAMILIAR * ((50+((PBEN_CONT_NORMAL +PBEN_CONT_NAO_EX ) *10))/100)); 
              ELSE
                 SERV_VAL_COTA_FAMILIAR := SERV_VAL_TOTAL_BENEFICIO;
              END IF;
          ELSE
            SERV_VAL_COTA_FAMILIAR := SERV_VAL_TOTAL_BENEFICIO  * ((50+((PBEN_CONT_NORMAL +PBEN_CONT_NAO_EX )*10))/100) ;
          END IF;
          
           SERV_VAL_COTA_FAMILIAR :=ROUND(TRUNC(SERV_VAL_COTA_FAMILIAR ,4),2);
 
        ---- Atualiza Dados do servidor pelo vinculo ----
         UPDATE TB_NPM_SERVIDOR PSER
                 SET PSER.VAL_TOTAL_BENEFICIO  =SERV_VAL_TOTAL_BENEFICIO, --- Valor Total
                     PSER.VAL_TOTAL_FINAL_PROP =SERV_VAL_COTA_FAMILIAR  , --- Cota Parte 
                     PSER.VAL_TETO_RGPS_PREV   =SERV_VVAL_TETO_RGPS_PREV, --- Tero RGPS
                     PSER.FLG_TETO_PREV_COMPL= SERV_FLG_TETO_PREV_COMP 
                 WHERE PSER.COD_INS      =   P_COD_INS        AND
                       PSER.COD_ADM_TRA  =   P_COD_ADM_TRA    AND
                        PSER.NUM_SEQ     =   P_MAX_NUM_SEQ;

   -------------------------------------------------------------
   ------------- Obtem rubrica para Lancamento -----------------

    SERV_RUBRICA_LEI1345 := SP_OBTEM_RUBRICA_LEI1354(P_COD_INS,SERV_FLG_TETO_PREV_COMP );

   ------- Grava composicao_ben_aux   -------------------
      SELECT NVL(COUNT(*) ,0)
        INTO
        QTD_BENEFICIO_AUX
      FROM TB_COMPOSICAO_BEN_AUX BAUX  
      WHERE BAUX.COD_INS       =  P_COD_INS    AND
            BAUX.COD_BENEFICIO =  P_COD_ADM_TRA;
 
   IF  QTD_BENEFICIO_AUX!=0  THEN
            SELECT NVL(COUNT(*) ,0)
              INTO
              QTD_BENEFICIO_AUX
            FROM TB_COMPOSICAO_BEN  CONC
            WHERE CONC.COD_INS       =  P_COD_INS    AND
                  CONC.COD_BENEFICIO =  P_COD_ADM_TRA;  

            IF    QTD_BENEFICIO_AUX =0 THEN
                   
               UPDATE TB_COMPOSICAO_BEN_AUX BAUX 
                    SET  BAUX.VAL_FIXO        = ROUND(TRUNC(SERV_VAL_TOTAL_BENEFICIO-  SERV_VVAL_TETO_RGPS_PREV,4),2),
                         BAUX.DAT_ULT_ATU     = SYSDATE             ,
                         BAUX.COD_FCRUBRICA   = SERV_RUBRICA_LEI1345,
                         BAUX.NOM_USU_ULT_ATU =  P_COD_USER         ,
                         BAUX.NOM_PRO_ULT_ATU = 'UPD_REGSTRA_DADOS  '
                    WHERE BAUX.COD_INS        =  P_COD_INS    AND
                          BAUX.COD_BENEFICIO  =  P_COD_ADM_TRA;
                
            END IF;  

   ELSE
     
                  INSERT  INTO TB_COMPOSICAO_BEN_AUX   ( 
                  cod_ins         ,
                  cod_beneficio   ,
                  cod_fcrubrica   ,  
                  seq_vig_fc      , 
                  seq_vig         ,
                  val_fixo        ,
                  val_porc        ,
                  val_inidade     ,
                  dat_ini_vig     ,
                  dat_fim_vig     ,
                  dat_ing         ,
                  dat_ult_atu     ,
                  nom_usu_ult_atu ,
                  nom_pro_ult_atu ,
                  cod_referencia  ,
                  val_str1        ,
                  val_str2        ,
                  val_porc2       ,
                  flg_status      ,
                  tipo_acao       ,
                  cod_entidade    ,
                  opcao_pccs      ,
                  cod_pccs        ,
                  cod_referencia_2,
                  des_informacao  ,
                  cod_tabela      ,
                  cod_funcao      ,
                  cod_cargo_incorp,
                  dat_incorp      ,
                  cod_cargo_2   
                  )
                  
                  VALUES
                 (
                  P_COD_INS       ,
                  P_COD_ADM_TRA   ,
                  SERV_RUBRICA_LEI1345  ,  
                  1               , 
                  1               ,
                  ROUND(TRUNC(SERV_VAL_TOTAL_BENEFICIO-  SERV_VVAL_TETO_RGPS_PREV,4),2),
                  NULL            ,
                  NULL            ,
                  SERV_DATA_INICIO_BENEFICIO ,    --dat_ini_vig     ,
                  NULL            , --dat_fim_vig     ,
                  sysdate         , --
                  sysdate         , --dat_ult_atu     ,
                  P_COD_USER      ,
                  'ACT_REGSTRA_DADOS', -- nom_pro_ult_atu ,
                  NULL            ,
                  NULL            , -- val_str1        ,
                  NULL            , -- val_str2        ,
                  NULL            , -- val_porc2       ,
                  'V'             , -- flg_status      ,
                  null            , -- tipo_acao       ,
                  P_COD_ENTIDADE  , -- cod_entidade    ,
                  null            , -- opcao_pccs      ,
                  null            , -- cod_pccs        ,
                  null            , -- cod_referencia_2,
                  null            , -- des_informacao  ,
                  null            , --cod_tabela      ,
                  null            , --cod_funcao      ,
                  null            , --cod_cargo_incorp,
                  null            , -- dat_incorp      ,
                  null              --cod_cargo_2                   
                  );
   
   END IF;
   ------------------- Beneficiarios atualizados -----------------------
          FOR BENEF_UPF IN (
          SELECT DISTINCT
                 PBENE.COD_ADM_TRA      ,
                 PBENE.COD_IDE_CLI_SERV ,
                 PBENE.COD_IDE_CLI_BEN
               FROM TB_NPM_BENEFICIARIO PBENE
               WHERE PBENE.COD_INS     = P_COD_INS     AND
                     PBENE.COD_ADM_TRA = P_COD_ADM_TRA
                     -------- Alterado em 27-07-2020 ---------
                     --- Deverea reservar cota do beneficiario
                      -- AND
                     --- PBENE.FLG_DEFERIR ='S'
        )
        LOOP
            ----- Numero de Sequencis de Beneficiario   
            SELECT MAX(PBENE2.NUM_SEQ) 
                   INTO   MAX_NUM_SEQ_BENEF 
                   FROM TB_NPM_BENEFICIARIO PBENE2
                   WHERE PBENE2.COD_INS      =   P_COD_INS        AND
                         PBENE2.COD_ADM_TRA  =   P_COD_ADM_TRA    AND
                         PBENE2.COD_IDE_CLI_SERV =BENEF_UPF.COD_IDE_CLI_SERV AND
                         PBENE2.COD_IDE_CLI_BEN  =BENEF_UPF.COD_IDE_CLI_BEN;         
  
          
                         SELECT   NVL(PBEN.FLG_POSSUI_BEN_PREV,'N'),
                                  NVL(PBEN.FLG_REDUCAO_CUMULATIVIDADE,'N'),
                                  PBEN.PROPORCAO,
                                  PBEN.COD_PERFIL_BENEFICIARIO  

                         INTO     PBEN_FLG_POSSUI_BEN_PREV,
                                  PBEN_FLG_OPCAO_BENEFICIO_VANTAJOSO,
                                  PBEN_PROPORCAO                    ,
                                  PBEN_COD_PERFIL   
                            FROM TB_NPM_BENEFICIARIO PBEN
                                   WHERE PBEN.COD_INS      =   P_COD_INS        AND
                                         PBEN.COD_ADM_TRA  =   P_COD_ADM_TRA    AND
                                         PBEN.NUM_SEQ      =   MAX_NUM_SEQ_BENEF    AND
                                         PBEN.COD_IDE_CLI_SERV =BENEF_UPF.COD_IDE_CLI_SERV AND
                                         PBEN.COD_IDE_CLI_BEN  =BENEF_UPF.COD_IDE_CLI_BEN;           
                         
                         IF /*PBEN_FLG_POSSUI_BEN_PREV           = 'S'  AND*/
                            PBEN_FLG_OPCAO_BENEFICIO_VANTAJOSO = 'S'   
                         THEN
                            
                              PBEN_BENEFICIO_ACUM  :=round(trunc(SERV_VAL_COTA_FAMILIAR *(PBEN_PROPORCAO/100)));
                              PBEN_BENEFICIO_ACUM  := SP_OBTEM_ACUMUATIVIDADE ( 
                                         P_DAT_CALC        ,
                                         round(trunc(SERV_VAL_COTA_FAMILIAR *(PBEN_PROPORCAO/100))), 
                                         OERRORCODE          ,
                                         OERRORMESSAGE     
                                         ); 
                          
                           IF PBEN_COD_PERFIL IN (8,801,803,590,80,90,63)  THEN 
                             UPDATE TB_NPM_BENEFICIARIO PBEN
                                       SET  
                                           PBEN.VAL_TOTAL_BENEFICIARIO= PBEN_BENEFICIO_ACUM 
                                       WHERE PBEN.COD_INS      =   P_COD_INS        AND
                                             PBEN.COD_ADM_TRA  =   P_COD_ADM_TRA    AND
                                             PBEN.NUM_SEQ      =   MAX_NUM_SEQ_BENEF    AND
                                             PBEN.COD_IDE_CLI_SERV =BENEF_UPF.COD_IDE_CLI_SERV AND
                                             PBEN.COD_IDE_CLI_BEN  =BENEF_UPF.COD_IDE_CLI_BEN;
                           ELSE

                            UPDATE TB_NPM_BENEFICIARIO PBEN
                                       SET  
                                           PBEN.VAL_TOTAL_BENEFICIARIO= PBEN_BENEFICIO_ACUM,
                                          PBEN.PROPORCAO              =PBEN_POR_TOT_BEN_PREV
                                       WHERE PBEN.COD_INS      =   P_COD_INS        AND
                                             PBEN.COD_ADM_TRA  =   P_COD_ADM_TRA    AND
                                             PBEN.NUM_SEQ      =   MAX_NUM_SEQ_BENEF    AND
                                             PBEN.COD_IDE_CLI_SERV =BENEF_UPF.COD_IDE_CLI_SERV AND
                                             PBEN.COD_IDE_CLI_BEN  =BENEF_UPF.COD_IDE_CLI_BEN;

                           
                           END IF;
            
                         ELSE
                              IF PBEN_COD_PERFIL IN (8,801,803,590,80,90,63)   THEN 
                                     UPDATE TB_NPM_BENEFICIARIO PBEN
                                               SET  
                                                   PBEN.VAL_TOTAL_BENEFICIARIO= round(trunc(SERV_VAL_COTA_FAMILIAR *(PBEN.PROPORCAO/100),4),2) 
                                                    
                                               WHERE PBEN.COD_INS      =   P_COD_INS        AND
                                                     PBEN.COD_ADM_TRA  =   P_COD_ADM_TRA    AND
                                                     PBEN.NUM_SEQ      =   MAX_NUM_SEQ_BENEF    AND
                                                     PBEN.COD_IDE_CLI_SERV =BENEF_UPF.COD_IDE_CLI_SERV AND
                                                     PBEN.COD_IDE_CLI_BEN  =BENEF_UPF.COD_IDE_CLI_BEN;
                              ELSE
                                    UPDATE TB_NPM_BENEFICIARIO PBEN
                                               SET  
                                                   PBEN.VAL_TOTAL_BENEFICIARIO= round(trunc(SERV_VAL_COTA_FAMILIAR *(PBEN.PROPORCAO/100),4),2)--,
                                                   --- Eliminado por Task de melhoria 67779 
                                                  --PBEN.PROPORCAO             =PBEN_POR_TOT_BEN_PREV
                                               WHERE PBEN.COD_INS      =   P_COD_INS        AND
                                                     PBEN.COD_ADM_TRA  =   P_COD_ADM_TRA    AND
                                                     PBEN.NUM_SEQ      =   MAX_NUM_SEQ_BENEF    AND
                                                     PBEN.COD_IDE_CLI_SERV =BENEF_UPF.COD_IDE_CLI_SERV AND
                                                     PBEN.COD_IDE_CLI_BEN  =BENEF_UPF.COD_IDE_CLI_BEN;
                              END IF;
            
                         END IF;
            
      
      END LOOP;
      -------- Atualiza % do Beneficiario Indeferido
       -------- Alterado em 27-07-2020 ---------
       -- Deverea reservar cota do beneficiario
 
     /*
        FOR BENEF_UPF IN (
              SELECT DISTINCT
                 PBENE.COD_ADM_TRA      ,
                 PBENE.COD_IDE_CLI_SERV ,
                 PBENE.COD_IDE_CLI_BEN
               FROM TB_NPM_BENEFICIARIO PBENE
               WHERE PBENE.COD_INS     = P_COD_INS     AND
                     PBENE.COD_ADM_TRA = P_COD_ADM_TRA  
        )
        LOOP     
              ----- Numero de Sequencis de Beneficiario   
            SELECT MAX(PBENE2.NUM_SEQ) 
                   INTO   MAX_NUM_SEQ_BENEF 
                   FROM TB_NPM_BENEFICIARIO PBENE2
                   WHERE PBENE2.COD_INS      =   P_COD_INS        AND
                         PBENE2.COD_ADM_TRA  =   P_COD_ADM_TRA    AND
                         PBENE2.COD_IDE_CLI_SERV =BENEF_UPF.COD_IDE_CLI_SERV AND
                         PBENE2.COD_IDE_CLI_BEN  =BENEF_UPF.COD_IDE_CLI_BEN;         
      
                         SELECT   NVL(PBEN.FLG_DEFERIR ,'N') 
                           INTO      
                              PBEN_FLG_DEFERIR     
                             FROM TB_NPM_BENEFICIARIO PBEN
                                   WHERE PBEN.COD_INS      =   P_COD_INS                   AND
                                         PBEN.COD_ADM_TRA  =   P_COD_ADM_TRA               AND
                                         PBEN.NUM_SEQ      =   MAX_NUM_SEQ_BENEF           AND
                                         PBEN.COD_IDE_CLI_SERV =BENEF_UPF.COD_IDE_CLI_SERV AND
                                         PBEN.COD_IDE_CLI_BEN  =BENEF_UPF.COD_IDE_CLI_BEN;           
                   IF  PBEN_FLG_DEFERIR ='N' THEN
  
                                    UPDATE TB_NPM_BENEFICIARIO PBEN
                                               SET  
                                                   PBEN.VAL_TOTAL_BENEFICIARIO= 0,
                                                   PBEN.PROPORCAO              = 0
                                               WHERE PBEN.COD_INS      =   P_COD_INS        AND
                                                     PBEN.COD_ADM_TRA  =   P_COD_ADM_TRA    AND
                                                     PBEN.NUM_SEQ      =   MAX_NUM_SEQ_BENEF    AND
                                                     PBEN.COD_IDE_CLI_SERV =BENEF_UPF.COD_IDE_CLI_SERV AND
                                                     PBEN.COD_IDE_CLI_BEN  =BENEF_UPF.COD_IDE_CLI_BEN;
  
                     
                   END IF;
                       
      
        END LOOP;
      */
      --------------------------
          
    EXCEPTION
       
      WHEN  v_erro  THEN
        OERRORCODE       := 10;
        OERRORMESSAGE    := 'Valor do Beneficio do protocolo esta Zerado';
      WHEN OTHERS THEN
        OERRORCODE       := 10;
        OERRORMESSAGE    := SQLERRM; 

     END;

   END;
 
  FUNCTION SP_VALIDA_TETO_COMPLEMETAR ( 
                                  P_COD_ENTIDADE     IN NUMBER   ,
                                  P_DAT_ING_SERV_PUB IN DATE    
                                 ) RETURN BOOLEAN AS
 Dat_ingress_vinculo  Date;
 Bol_gera_redutor     Boolean  :=FALSE;
 
 BEGIN
 
        Dat_ingress_vinculo:=P_DAT_ING_SERV_PUB;
                  /*
                      -- 54 (usp)                        02/10/2013  
                      -- 53 (unicamp)                    02/10/2013 
                      -- 52 (unesp)                      02/10/2013 
                      -- 50 (alesp)                      22/03/2013  
                      -- 46 (tribunal de justi?a)        23/06/2014
                      -- 48 (TJM)                        23/06/2014
                      -- 49 (ministerio publico)         23/06/2014
                      -- 51 (tribunal de contas)         23/06/2014
                      --  3 Defesoria                    23/06/2014 
                      --  Todas menos (Policia iilitar)  21/01/2013   
                   */   
        
                       CASE
                              When  P_COD_ENTIDADE IN (52 ,53, 54)  AND 
                                  Dat_ingress_vinculo  >= TO_DATE ('02/10/2013','DD/MM/YYYY') THEN
                                  BEGIN 
                                    Bol_gera_redutor:=TRUE;                                 
                                  END; 
                               When  P_COD_ENTIDADE = 50 AND 
                                  Dat_ingress_vinculo  >= TO_DATE ('22/03/2013','DD/MM/YYYY') THEN
                                  BEGIN 
                                     Bol_gera_redutor:=TRUE;
                                  END; 
 
                               When  P_COD_ENTIDADE IN (3,46,48,49,51) AND 
                                  Dat_ingress_vinculo  >= TO_DATE ('02/10/2013','DD/MM/YYYY') THEN
                                  BEGIN 
                                     Bol_gera_redutor:=TRUE;
                                  END; 
                               When  P_COD_ENTIDADE !=5 AND 
                                  Dat_ingress_vinculo  >= TO_DATE ( '21/01/2013','DD/MM/YYYY') THEN
                                  BEGIN 
                                     Bol_gera_redutor:=TRUE;
                                  END; 
                               ELSE
                                  Bol_gera_redutor:=FALSE;
                          END CASE;
           
        RETURN  Bol_gera_redutor;
  END; 
  FUNCTION SP_OBTEM_VARIAVEL ( 
                              P_COD_PARAM      IN VARCHAR,
                              P_COD_ESTRUTURA  IN NUMBER ,
                              P_COD_ELEMENTO   IN VARCHAR,
                              P_DAT_CALCULO    IN  DATE 
                              ) RETURN NUMBER AS
 
  VAL_VARIAVEL   NUMBER(18,6):=0;
  BEGIN
          BEGIN
                           ----- OBTEM TETO DO RGPS -------
                           SELECT NVL(MAX(P.VAL_ELEMENTO),0)
                             INTO VAL_VARIAVEL
                             FROM TB_DET_PARAM_ESTRUTURA P
                            WHERE P.COD_PARAM      =  P_COD_PARAM       --'TASCO'
                              AND P.COD_ESTRUTURA  =  P_COD_ESTRUTURA  --2000
                              AND P.COD_ELEMENTO   =  P_COD_ELEMENTO    -- 'DESC_CONTR'
                              AND P.INI_VIG              < TRUNC(P_DAT_CALCULO)
                              AND TRUNC(P_DAT_CALCULO)   <= NVL(P.FIM_VIG,P_DAT_CALCULO);
         EXCEPTION
              WHEN OTHERS THEN
                    VAL_VARIAVEL :=0;
          END;
              RETURN  VAL_VARIAVEL;      
 END;

PROCEDURE SP_COMPOSICAO_BASE(
                          P_COD_INS        IN  NUMBER  ,
                          P_COD_RUBRICA    IN  NUMBER  ,
                          P_VARIAVEL       IN  VARCHAR2,
                          P_COD_ADM_TRA    IN  NUMBER  ,
                          P_COD_ENTIDADE   IN  NUMBER  ,
                          P_PAR_PER_PRO    IN  DATE    ,
                          O_VALOR          OUT NUMBER  ,
                          OERRORCODE       OUT NUMBER  ,
                          OERRORMESSAGE    OUT VARCHAR                               
                          ) IS
  
   ---- Variaveis ------
     VI_VALOR    NUMBER(18, 5);
     VI_RUBRICA  NUMBER;
     M           NUMBER := 0;
     V_SOMA      CHAR(1) := 'S';

  BEGIN
     O_VALOR  := 0;
     VI_VALOR := 0;
 
   BEGIN
      SELECT           
             SUM(DECODE(RUB.FLG_NATUREZA,'C',CTRA.VAL_RUBRICA,CTRA.VAL_RUBRICA*-1)) 
          INTO    O_VALOR 
          FROM TB_COMPOE_DET CD     ,
               TB_COMP_SAL_TRA CTRA , 
               TB_RUBRICAS     RUB
       WHERE CD.COD_INS                 = P_COD_INS
         AND CD.COD_FCRUBRICA_COMPOSTA  = P_COD_RUBRICA
         AND CD.COD_VARIAVEL            = P_VARIAVEL
         AND CD.COD_ENTIDADE_COMPOSTA   = P_COD_ENTIDADE
         AND CD.COD_FCRUBRICA_COMPOE    = CTRA.COD_FCRUBRICA
         AND (P_PAR_PER_PRO >= CD.DAT_INI_VIG AND
              P_PAR_PER_PRO <=
             NVL(CD.DAT_FIM_VIG, TO_DATE('01/01/2045', 'dd/mm/yyyy')))
         AND CTRA.COD_INS       =P_COD_INS
         AND CTRA.COD_ADM_TRA   =P_COD_ADM_TRA
         AND CTRA.COD_FCRUBRICA =CD.COD_FCRUBRICA_COMPOE
         AND RUB.COD_INS        =P_COD_INS
         AND RUB.COD_RUBRICA    =CD.COD_FCRUBRICA_COMPOE
         AND RUB.COD_ENTIDADE   =P_COD_ENTIDADE;
     EXCEPTION
            WHEN NO_DATA_FOUND  THEN
               NULL;
            WHEN OTHERS THEN
             OERRORCODE      :=14;
             OERRORMESSAGE   :='Erro ao obter base de composicao';            
     END;
 


   END SP_COMPOSICAO_BASE;
   
  FUNCTION SP_OBTEM_ACUMUATIVIDADE ( 
                              P_DAT_CALCULO      IN  DATE   ,
                              P_VALOR_BENEFICIO  IN  NUMBER , 
                              OERRORCODE         OUT NUMBER ,
                              OERRORMESSAGE      OUT VARCHAR                               
   ) RETURN NUMBER AS
   

  ACUM_FAIXA1    NUMBER  (18,4);
  ACUM_FAIXA2    NUMBER  (18,4);
  ACUM_FAIXA3    NUMBER  (18,4);
  ACUM_FAIXA4    NUMBER  (18,4);
  ACUM_FAIXA5    NUMBER  (18,4);

  ACUM_VALO_BEN   NUMBER (18,4);
  ACUM_SAL_MIN    NUMBER (18,4);
  
  
  BEGIN
       
      --- Obtem Valor de Salario Minimo 
      ACUM_SAL_MIN := SP_OBTEM_VARIAVEL ( 
                              'SALMIN'     , 
                              1000         , 
                              'PAR_SAL_MIN',
                              P_DAT_CALCULO  
                              );



      ACUM_FAIXA1     :=ACUM_SAL_MIN  ;
      ACUM_FAIXA2     :=ACUM_SAL_MIN  * 2 ;
      ACUM_FAIXA3     :=ACUM_SAL_MIN  * 3 ;
      ACUM_FAIXA4     :=ACUM_SAL_MIN  * 4 ;
      ACUM_FAIXA5     :=ACUM_SAL_MIN  * 5 ;
      
      IF    P_VALOR_BENEFICIO > ACUM_FAIXA1 THEN
            IF P_VALOR_BENEFICIO > ACUM_FAIXA2 THEN
               IF  P_VALOR_BENEFICIO > ACUM_FAIXA3 THEN
                    IF  P_VALOR_BENEFICIO > ACUM_FAIXA4 THEN
                        ACUM_VALO_BEN :=ACUM_FAIXA1   + (P_VALOR_BENEFICIO - ACUM_FAIXA4)* 10/100;
                        ACUM_VALO_BEN :=ACUM_VALO_BEN + (ACUM_FAIXA1 * 20/100);
                        ACUM_VALO_BEN :=ACUM_VALO_BEN + (ACUM_FAIXA1 * 40/100);
                        ACUM_VALO_BEN :=ACUM_VALO_BEN + (ACUM_FAIXA1 * 60/100);
                         
                         
                    ELSE
                        ACUM_VALO_BEN :=ACUM_FAIXA1   + (P_VALOR_BENEFICIO - ACUM_FAIXA3)* 20/100;
                        ACUM_VALO_BEN :=ACUM_VALO_BEN + (ACUM_FAIXA1 * 40/100);
                        ACUM_VALO_BEN :=ACUM_VALO_BEN + (ACUM_FAIXA1 * 60/100);
                    END IF;
               
               ELSE
                        ACUM_VALO_BEN :=ACUM_FAIXA1   + (P_VALOR_BENEFICIO - ACUM_FAIXA2)* 40/100;
                        ACUM_VALO_BEN :=ACUM_VALO_BEN + (ACUM_FAIXA1 * 60/100);
 
               END IF; 
            
            ELSE
              --- Faixa 2 ---
                              ACUM_VALO_BEN :=ACUM_FAIXA1    + (P_VALOR_BENEFICIO - ACUM_FAIXA1)* 60/100;
 
            END IF;
      ELSE
         -- Faixa 1 
         ACUM_VALO_BEN :=P_VALOR_BENEFICIO;
      END IF;
      RETURN ACUM_VALO_BEN ;
  
  
  END;
   
   PROCEDURE  SP_FORMA_PENSAO (
         P_COD_INS      IN NUMBER       ,
         P_COD_ADM_TRA  IN VARCHAR2     ,
         P_COD_REGISTRO IN NUMBER       ,
         P_COD_USER     IN VARCHAR      ,
         P_COD_ERRO    OUT NUMBER       ,
         P_MSG_ERRO    OUT VARCHAR2  
    ) AS
  
 /****** variaveis de Erro ****/
  v_erro_tipo1701            exception;
  v_erro_tipo1702            exception;
  v_erro_tipo1703            exception;
 
 
 ------- Varioaves de apoio-------
  RGDAD_IND_TIP_CALC       number;
  MAX_NUM_SEQ              number;
  QTD_REG_GRAVADOS         number;
  
  BEGIN  
  
     BEGIN
       
      FOR REG_FORM IN (
        
               SELECT  P.TRP_COD_ADM_TRA      AS COD_ADM_TRA  FROM SIMTRP P 
                WHERE P.TRP_COD_ADM_TRA  = P_COD_ADM_TRA
                UNION  
                SELECT    P.TRP_COD_TRA_COMPL AS COD_ADM_TRA   FROM SIMTRP P 
                WHERE P.TRP_COD_ADM_TRA    =  P_COD_ADM_TRA AND
                      P.TRP_COD_TRA_COMPL  != P.TRP_COD_ADM_TRA 
       )
       LOOP 
      
           --- Obtem sequencia a atualizar 
          MAX_NUM_SEQ :=0;
          SELECT MAX(PSER.NUM_SEQ) 
                 INTO   MAX_NUM_SEQ 
                 FROM TB_NPM_SERVIDOR PSER
                 WHERE PSER.COD_INS      =   P_COD_INS        AND
                       PSER.COD_ADM_TRA  =   REG_FORM.COD_ADM_TRA    AND
                       PSER.COD_REGISTRO =   P_COD_REGISTRO ;
     
            IF NVL(MAX_NUM_SEQ  ,0) > 0 THEN
                       SELECT
                             S.TRP_SEQ           ,
                             S.TRP_FEC_SOL_TRA   ,
                             S.TRP_IDE_CLI       ,
                             S.TRP_IND_TIP_TRA   ,
                             S.TRP_COD_ADM_TRA   ,
                             S.TRP_IDE_PER       ,
                             S.TRP_NUM_MAT       ,
                             S.TRP_COD_TRA_COMPL ,
                             S.TRP_IDE_REL_FUNC  ,
                             S.TRP_COD_ENTIDADE  ,
                             S.TRP_CARGO_PEN     ,
                             SER.COD_TAREFA      ,
                             SER.DAT_ING_SERV_PUB,
                             FI.DAT_OBITO        ,
                             S.TRP_FEC_FIN_PUB   ,
                             SER.VAL_TOTAL_FINAL_PROP 
                             
                        INTO PAR_TRP_SEP         , 
                             PAR_DT_SOLICITACAO  ,
                             PAR_COD_IDE_CLI     ,
                             PAR_TIP_BEN         ,
                             PAR_COD_ADM_TRA     ,
                             PAR_IDE_PER         ,
                             PAR_NUM_MAT         ,
                             PAR_COD_TRA_COMPL   ,
                             PAR_IDE_REL_FUNC    ,
                             PAR_COD_ENTIDADE    ,
                             PAR_COD_CARGO       ,
                             PAR_COD_TAREFA      ,
                             PAR_DAT_ING_SERV_PUB,
                             PAR_DATA_OBITO      ,
                             PAR_DAT_PUBLICACAO  ,
                             PAR_VAL_BENEFICIO    
                                   
                      FROM SIMTRP S, TB_NPM_SERVIDOR SER, TB_PESSOA_FISICA FI
                      WHERE S.TRP_COD_ADM_TRA  = P_COD_ADM_TRA
                        AND S.TRP_COD_INS      = P_COD_INS
                        AND SER.COD_INS        = P_COD_INS
                        AND SER.COD_ADM_TRA    = REG_FORM.COD_ADM_TRA
                        AND SER.COD_REGISTRO   = P_COD_REGISTRO
                        AND SER.NUM_SEQ        = MAX_NUM_SEQ
                        AND FI.COD_INS         = P_COD_INS
                        AND FI.COD_IDE_CLI     = SER.COD_IDE_CLI_SERV;

                  --- Insere dados de concessao de beneficio  ---
                   SELECT COUNT(*) 
                   INTO   QTD_REG_GRAVADOS 
                   FROM   TB_CONCESSAO_BENEFICIO  CONC
                   WHERE CONC.COD_INS       =   P_COD_INS               AND
                         CONC.COD_BENEFICIO =   REG_FORM.COD_ADM_TRA    AND
                         CONC.COD_TIPO_BENEFICIO='M';    
  
                  --- Obtem PCCS   ----
                  SELECT
                     FUNC.COD_PCCS   ,  
                     ENT.ORG_LEGADOR ,
                     ENT.UO_LEGADOR   
                     INTO PAR_COD_PCCS   ,
                          PAR_ORG_LEGADOR,
                          PAR_UO_LEGADOR  
                     FROM TB_RELACAO_FUNCIONAL FUNC, TB_ENTIDADE_FOLHA ENT 
                     WHERE FUNC.COD_INS          = P_COD_INS         AND
                           FUNC.COD_IDE_CLI      = PAR_COD_IDE_CLI   AND
                           FUNC.NUM_MATRICULA    = PAR_NUM_MAT       AND
                           FUNC.COD_IDE_REL_FUNC = PAR_IDE_REL_FUNC  AND
                           FUNC.COD_ENTIDADE     = PAR_COD_ENTIDADE  AND
                           ENT.COD_ENTIDADE_FGV  = PAR_COD_ENTIDADE  ;
            
                  --- Obtem Dados do Rregra da concessao  =--
                   SELECT 
                     PAP.PAP_IND_TIP_CALC ,
                     PAP.PAP_SEQ
                     INTO 
                     PAR_IND_TIP_CALC     ,
                     PAR_PAP_SEQ  
                     FROM SIMPAP PAP
                     WHERE PAP.PAP_COD_INS     = P_COD_INS       AND
                           PAP.PAP_IDE_CLI     = PAR_COD_IDE_CLI AND
                           PAP.PAP_COD_ADM_TRA = REG_FORM.COD_ADM_TRA;
                                            
                   IF QTD_REG_GRAVADOS = 0 THEN
                      INSERT INTO TB_CONCESSAO_BENEFICIO (
                                    cod_ins ,
                                    cod_beneficio ,
                                    cod_tipo_beneficio  ,
                                    cod_ide_cli_serv  ,
                                    cod_entidade  ,
                                    cod_pccs  ,
                                    cod_cargo ,
                                    num_matricula ,
                                    cod_ide_rel_func  ,
                                    num_proc_ben  ,
                                    dat_solicitacao ,
                                    dat_concessao ,
                                    dat_pub ,
                                    dat_efeito  ,
                                    val_beneficio ,
                                    val_percent_ben ,
                                    flg_reg_ativ  ,
                                    flg_status  ,
                                    dat_ing ,
                                    dat_ult_atu ,
                                    nom_usu_ult_atu ,
                                    nom_pro_ult_atu ,
                                    cod_cargo_apos  ,
                                    val_perc_pecunia  ,
                                    flg_op_3perc  ,
                                    cod_beneficio_ext ,
                                    seq_ben ,
                                    cod_tipo_calculo  ,
                                    teto_fixo ,
                                    cod_org_legador ,
                                    cod_uo_legador  ,
                                    cod_uadm  ,
                                    perc_pens ,
                                    prop   
                      
                      )
                      VALUES
                      (
                                    P_COD_INS             , -- cod_ins
                                    REG_FORM.COD_ADM_TRA  , -- cod_beneficio 
                                    'M'                   , -- cod_tipo_beneficio  
                                    PAR_COD_IDE_CLI       , -- cod_ide_cli_serv  
                                    PAR_COD_ENTIDADE      , -- cod_entidade   
                                    PAR_COD_PCCS          , -- cod_pccs  
                                    PAR_COD_CARGO         , -- cod_cargo 
                                    PAR_NUM_MAT           , -- num_matricula 
                                    PAR_IDE_REL_FUNC      , -- cod_ide_rel_func  
                                    100                   , -- num_proc_ben  
                                    PAR_DT_SOLICITACAO    , -- dat_solicitacao 
                                    PAR_DT_SOLICITACAO    , -- dat_concessao *** CORREGIR***
                                    PAR_DAT_PUBLICACAO    , -- dat_pub   
                                    PAR_DAT_PUBLICACAO    , -- dat_efeito   *** CORREGIR***
                                    PAR_VAL_BENEFICIO     , -- val_beneficio  
                                    100                   , -- val_percent_ben  
                                    'S'                   , -- flg_reg_ativ   
                                    'V'                   , -- flg_status   
                                    sysdate               , -- dat_ing 
                                    sysdate               , -- dat_ult_atu  
                                    P_COD_USER            , -- nom_usu_ult_atu 
                                    'Ingressa_formaliza'  , -- nom_pro_ult_atu 
                                    null                  , -- cod_cargo_apos  
                                    100                   , -- val_perc_pecunia   
                                    null                  , -- flg_op_3perc  
                                    REG_FORM.COD_ADM_TRA  , -- cod_beneficio_ext  
                                    PAR_PAP_SEQ           , -- seq_ben ,
                                    PAR_PAP_SEQ           , -- cod_tipo_calculo  ,
                                    0                     , -- teto_fixo ,
                                    PAR_ORG_LEGADOR       , -- cod_org_legador
                                    PAR_UO_LEGADOR        , -- cod_org_legador     
                                    NULL                  , -- cod_uadm  ,
                                    NULL                  , -- perc_pens ,
                                    NULL                    -- prop                         
                      );
                   
                   END IF; 

             ELSE
                   RAISE  v_erro_tipo1701;
             END IF;
             
             
     END LOOP;
     EXCEPTION
        When  v_erro_tipo1701 Then
          P_COD_ERRO    := 17;
          P_MSG_ERRO    := 'N?o foram encontrados dados'  ;

        When OTHERS Then
          P_COD_ERRO    := 18;
          P_MSG_ERRO    := 'N?o foram encontrados dados'  ;   
        END;
    END; 
   
   
   PROCEDURE  SP_RECALCULA_COMPSALTRA (
         P_COD_INS      IN NUMBER       ,
         P_COD_ADM_TRA  IN VARCHAR2     ,
         P_COD_REGISTRO IN NUMBER       ,
         P_COD_USER     IN VARCHAR      ,
         P_COD_ERRO    OUT NUMBER       ,
         P_MSG_ERRO    OUT VARCHAR2  
    ) AS
  
 /****** variaveis de Erro ****/
  v_erro_tipo1701            exception;
  v_erro_tipo1702            exception;
  v_erro_tipo1703            exception;
 
  W_COD_TRAMITE               VARCHAR2(10);
 ------- Varioaves de apoio-------
  RGDAD_IND_TIP_CALC       number;
  MAX_NUM_SEQ              number;
  QTD_REG_GRAVADOS         number;
  
  
  BEGIN  
  
     BEGIN
       
      FOR REG_RCAL IN (
        
               SELECT  P.TRP_COD_ADM_TRA      AS COD_ADM_TRA  FROM SIMTRP P 
                WHERE P.TRP_COD_ADM_TRA  = P_COD_ADM_TRA
                UNION  
                SELECT    P.TRP_COD_TRA_COMPL AS COD_ADM_TRA   FROM SIMTRP P 
                WHERE P.TRP_COD_ADM_TRA    =  P_COD_ADM_TRA AND
                      P.TRP_COD_TRA_COMPL  != P.TRP_COD_ADM_TRA 
       )
       LOOP 
 
           W_COD_TRAMITE:=REG_RCAL.COD_ADM_TRA;
            --- Obtem sequencia a atualizar 
          MAX_NUM_SEQ :=0;
          SELECT MAX(PSER.NUM_SEQ) 
                 INTO   MAX_NUM_SEQ 
                 FROM TB_NPM_SERVIDOR PSER
                 WHERE PSER.COD_INS      =   P_COD_INS        AND
                       PSER.COD_ADM_TRA  =   REG_RCAL.COD_ADM_TRA    AND
                       PSER.COD_REGISTRO =   P_COD_REGISTRO ;     
 
                       SELECT
                             S.TRP_SEQ           ,
                             S.TRP_FEC_SOL_TRA   ,
                             S.TRP_IDE_CLI       ,
                             S.TRP_IND_TIP_TRA   ,
                             S.TRP_COD_ADM_TRA   ,
                             S.TRP_IDE_PER       ,
                             S.TRP_NUM_MAT       ,
                             S.TRP_COD_TRA_COMPL ,
                             S.TRP_IDE_REL_FUNC  ,
                             S.TRP_COD_ENTIDADE  ,
                             S.TRP_CARGO_PEN     ,
                             SER.COD_TAREFA      ,
                             SER.DAT_ING_SERV_PUB,
                             FI.DAT_OBITO        ,
                             S.TRP_FEC_FIN_PUB   ,
                             SER.VAL_TOTAL_FINAL_PROP 
                             
                        INTO PAR_TRP_SEP         , 
                             PAR_DT_SOLICITACAO  ,
                             PAR_COD_IDE_CLI     ,
                             PAR_TIP_BEN         ,
                             PAR_COD_ADM_TRA     ,
                             PAR_IDE_PER         ,
                             PAR_NUM_MAT         ,
                             PAR_COD_TRA_COMPL   ,
                             PAR_IDE_REL_FUNC    ,
                             PAR_COD_ENTIDADE    ,
                             PAR_COD_CARGO       ,
                             PAR_COD_TAREFA      ,
                             PAR_DAT_ING_SERV_PUB,
                             PAR_DATA_OBITO      ,
                             PAR_DAT_PUBLICACAO  ,
                             PAR_VAL_BENEFICIO    
                                   
                      FROM SIMTRP S, TB_NPM_SERVIDOR SER, TB_PESSOA_FISICA FI
                      WHERE S.TRP_COD_ADM_TRA  = P_COD_ADM_TRA
                        AND S.TRP_COD_INS      = P_COD_INS
                        AND SER.COD_INS        = P_COD_INS
                        AND SER.COD_ADM_TRA    = REG_RCAL.COD_ADM_TRA
                        AND SER.COD_REGISTRO   = P_COD_REGISTRO
                        AND SER.NUM_SEQ        = MAX_NUM_SEQ
                        AND FI.COD_INS         = P_COD_INS
                        AND FI.COD_IDE_CLI     = SER.COD_IDE_CLI_SERV;

    
  
                  --- Obtem PCCS   ----
                  SELECT
                     FUNC.COD_PCCS   ,  
                     ENT.ORG_LEGADOR ,
                     ENT.UO_LEGADOR   
                     INTO PAR_COD_PCCS   ,
                          PAR_ORG_LEGADOR,
                          PAR_UO_LEGADOR  
                     FROM TB_RELACAO_FUNCIONAL FUNC, TB_ENTIDADE_FOLHA ENT 
                     WHERE FUNC.COD_INS          = P_COD_INS         AND
                           FUNC.COD_IDE_CLI      = PAR_COD_IDE_CLI   AND
                           FUNC.NUM_MATRICULA    = PAR_NUM_MAT       AND
                           FUNC.COD_IDE_REL_FUNC = PAR_IDE_REL_FUNC  AND
                           FUNC.COD_ENTIDADE     = PAR_COD_ENTIDADE  AND
                           ENT.COD_ENTIDADE_FGV  = PAR_COD_ENTIDADE  AND
                           ROWNUM <2;
 
 
 
                       PAC_CALCULO_VALOR_RUBRICAS.PAC_CALCULO_RUBRICAS_NL(
                                  P_COD_INS          , 
                                  TO_DATE('01/'||TO_CHAR(PAR_DATA_OBITO,'MM/YYYY'),'DD/MM/YYYY'),--PERIODOFOLHA,
                                  P_COD_USER           ,
                                  REG_RCAL.COD_ADM_TRA ,
                                  PAR_COD_IDE_CLI      ,
                                  'M'                  ,  
                                  PAR_DATA_OBITO       , 
                                  PAR_DAT_ING_SERV_PUB, 
                                  OERRORMESSAGE);
            
   
                         INSERT INTO   USER_IPESP.TB_NPM_LOG_TAREFA
                                (
                                  cod_ins      ,   
                                  cod_adm_tra  , 
                                  cod_registro , 
                                  gls_param    , 
                                  dat_ing       
                                )
                         VALUES
                                (
                                P_COD_INS           ,
                                REG_RCAL.COD_ADM_TRA,
                                P_COD_REGISTRO      ,
                                REG_RCAL.COD_ADM_TRA          || '-'||
                                TO_CHAR(P_COD_REGISTRO)       || '-'||
                                TO_CHAR(TO_DATE('01/'||TO_CHAR(PAR_DATA_OBITO,'MM/YYYY'),'DD/MM/YYYY'))||'-'||
                                TO_CHAR(PAR_DAT_ING_SERV_PUB) || '-'||
                                SUBSTR(OERRORMESSAGE,1,150),
                                SYSDATE
                                );  
  
  
            
             
     END LOOP;
                   ---- Atualiza % da aposentadoria legado
              UPDATE TB_NPM_SERVIDOR NPM_SER
                     SET  NPM_SER.DAT_ULT_ATU    = SYSDATE,
                          NPM_SER.NOM_PRO_ULT_ATU='CALCULA_COMPSALTRA'
                     WHERE NPM_SER.COD_INS      =     P_COD_INS       AND
                           NPM_SER.COD_ADM_TRA  =     P_COD_ADM_TRA   AND
                           NPM_SER.COD_REGISTRO =     P_COD_REGISTRO ;
                           
     EXCEPTION
        When  v_erro_tipo1701 Then
          P_COD_ERRO    := 17;
          P_MSG_ERRO    := 'Semforam encontrados dados'  ;

        When OTHERS Then
          P_COD_ERRO    := 18;
          P_MSG_ERRO    := SQLERRM ; --'Sem foram encontrados dados  -'||to_char(W_COD_TRAMITE )   ;   
        END;
    END; 
  
  PROCEDURE SP_CALCULA_MEDIA_PENSAO
  (
         P_COD_INS              IN NUMBER       ,
         P_COD_ADM_TRA_VINCULO  IN VARCHAR2     ,
         P_COD_REGISTRO         IN NUMBER       ,   
         P_COD_USER             IN VARCHAR      ,
         P_COD_ERRO             OUT NUMBER      ,
         P_MSG_ERRO             OUT VARCHAR2
  ) AS
  

   -------   Dados de Calculo  -----
  CGPA_flg_faleceu_atividade                    VARCHAR2(1);
  CGPA_flg_obito_decorrente_exerc_raza          VARCHAR2(1);
  CGPA_cod_beneficio_faleceu_atividade          NUMBER(8); 
  CGPA_flg_faleceu_acid_doenca                  VARCHAR2(1);
  CGPA_IND_TIP_CALC                             VARCHAR2(1);
  CGPA_POR_RETRIB                               NUMBER(8,4);
  CGPA_QTD_TMP_CONTR_DIAS_TOTAL                 NUMBER(8);
  CGPA_IND_TIP_COB                              VARCHAR2(1);      
  CGPA_POR_CALCULO                              NUMBER(8,4);
  CGPA_COD_TAREFA                               NUMBER(8);  
  CGPA_COD_REGISTRO                             NUMBER(8); 
  CGPA_SEQBEN                                   NUMBER(8);
  
  BEGIN 
      BEGIN
        --- Obtenedo regra principal de pens?o por morte.   
         SELECT  RB.SEQ_BEN                           , 
                 PSE.COD_IDE_CLI_SERV                 ,
                 PSE.NUM_MATRICULA                    ,
                 PSE.COD_IDE_REL_FUNC                 ,
                 PSE.COD_ENTIDADE                     ,
                 PSE.FLG_FALECEU_ATIVIDADE            ,
                 PSE.FLG_OBITO_DECORRENTE_EXERC_RAZAO ,
                 PSE.COD_BENEFICIO_FALECEU_ATIVIDADE  ,
                 PSE.FLG_FALECEU_ACID_DOENCA          ,
                 PSE.QTD_TMP_CONTR_DIAS_TOTAL         , 
                 SERV.DAT_OBITO                       ,
                 PSE.DAT_ING_SERV_PUB                 ,          
                 PSE.COD_TAREFA                       ,
                 PSE.COD_REGISTRO
                 INTO   GPAP_SEQBEN                             ,
                          GPAP_COD_IDE_CLI_SERV                 ,
                          GPAP_NUM_MATRICULA                    ,
                          GPAP_COD_IDE_REL_FUNC                 ,
                          GPAP_COD_ENTIDADE                     ,                             
                          CGPA_flg_faleceu_atividade            ,
                          CGPA_flg_obito_decorrente_exerc_raza  ,
                          CGPA_cod_beneficio_faleceu_atividade  ,
                          CGPA_flg_faleceu_acid_doenca          ,  
                          CGPA_QTD_TMP_CONTR_DIAS_TOTAL         ,
                          CGPA_DAT_OBITO                        ,
                          CGPA_DAT_ING_SERV_PUB                 ,
                          CGPA_COD_TAREFA                       , 
                          CGPA_COD_REGISTRO                        
                 FROM TB_REGRA_CONC_BEN              RB  ,
                           TB_REGRA_BENEF            RBE ,
                           TB_NPM_SERVIDOR           PSE ,
                           TB_PESSOA_FISICA          SERV 
            WHERE RB.COD_INS          = P_COD_INS                        AND
                  RB.COD_ESTRUTURA    = 1                                AND
                  RB.COD_TIP_REGRA    = 2                                AND 
                  SERV.COD_INS        = P_COD_INS                        AND  
                  SERV.COD_IDE_CLI    = PSE.COD_IDE_CLI_SERV             AND
                  SERV.DAT_OBITO  >=RB.DAT_INI_VIG                       AND   
                  SERV.DAT_OBITO  <=NVL(RB.DAT_FIM_VIG ,SERV.DAT_OBITO ) AND   
                  RBE.COD_INS         = P_COD_INS                        AND
                  RBE.SEQ_BEN         = RB.SEQ_BEN                       AND
                  PSE.COD_INS         = P_COD_INS                        AND
                  PSE.COD_REGISTRO    = P_COD_REGISTRO                   AND
                  PSE.COD_ADM_TRA     = P_COD_ADM_TRA_VINCULO            AND
                  RB.SEQ_BEN          = 202001;
                
                
      EXCEPTION
       When OTHERS Then
        RAISE  v_erro_tipo1;
      END;
      
        BEGIN
         SELECT 
               PAP.PAP_IND_TIP_CALC
           INTO 
               CGPA_IND_TIP_CALC 
         FROM SIMPAP PAP
         WHERE PAP.PAP_COD_INS     = P_COD_INS              AND
               PAP.PAP_COD_ADM_TRA = P_COD_ADM_TRA_VINCULO  AND
               PAP.PAP_IDE_CLI     = GPAP_COD_IDE_CLI_SERV;
         EXCEPTION
         When OTHERS Then
            RAISE  v_erro_tipo1;
          END;  
                      
       BEGIN          

              SP_LIMPA_ADM_MEDIA  (
                  P_COD_INS             ,
                  P_COD_ADM_TRA_VINCULO ,
                  P_COD_REGISTRO        ,
                  OERRORCODE            ,
                  OERRORMESSAGE                                     
              ); 
              
             IF CGPA_IND_TIP_CALC  ='5'  THEN
                    --- Calculo de 60 % + 2 % por ano
                  CGPA_POR_CALCULO  := TRUNC(CGPA_QTD_TMP_CONTR_DIAS_TOTAL/365)- 20;
                  IF  CGPA_POR_CALCULO > 0 THEN
                     CGPA_POR_CALCULO  := 60 + TRUNC((TRUNC(CGPA_QTD_TMP_CONTR_DIAS_TOTAL/365)- 20)*2);
                  ELSE
                     CGPA_POR_CALCULO  := 60;
                  END IF; 
              ELSE
                    CGPA_POR_CALCULO  := 100;   
              END IF;          
               
              IF nvl(OERRORCODE,0)  =0 THEN   
                    SP_OBTEM_DADOS_APOSENTADORIA_REGRA3
                               (P_COD_INS             ,
                                P_COD_ADM_TRA_VINCULO ,
                                CGPA_DAT_OBITO        ,
                                GPAP_COD_IDE_CLI_SERV ,
                                GPAP_NUM_MATRICULA    ,
                                GPAP_COD_IDE_REL_FUNC ,
                                GPAP_COD_ENTIDADE     , 
                                100                   ,  
                                '05'                  , 
                                CGPA_POR_CALCULO      , 
                                CGPA_DAT_ING_SERV_PUB ,                                 
                                CGPA_COD_TAREFA       , 
                                CGPA_COD_REGISTRO     ,
                                P_COD_USER            ,  
                                GPAP_SEQBEN           ,                                         
                                OERRORCODE            ,
                                OERRORMESSAGE        ) ;  
                    CGPA_IND_TIP_COB :='P';                                   
                    CGPA_POR_RETRIB  := CGPA_POR_CALCULO;                        
              END IF;    
           END;              
    
   EXCEPTION
      WHEN v_erro_tipo1 THEN
        P_COD_ERRO    := 05;
        P_MSG_ERRO    := 'Nao foram encontradas dados para recalculo'  ;


      WHEN OTHERS THEN
        P_COD_ERRO    := 99;
        P_MSG_ERRO    := OERRORMESSAGE;
                   
  END;
  FUNCTION SP_OBTEM_RUBRICA_LEI1354 ( 
                                  P_COD_INS          IN NUMBER   ,
                                  P_BOL_GERA_REDUTOR IN CHAR     
                                 ) RETURN NUMBER AS
 
 V_RUBRICA_LEI1354  NUMBER:=0;
 V_TIPO             NUMBER:=2;
 BEGIN
   
     IF  P_BOL_GERA_REDUTOR='S'  THEN
          V_TIPO :=2;
     ELSE
          V_TIPO :=1;
     END IF;
     
     BEGIN 
      SELECT TO_NUMBER( COD.DES_DESCRICAO)
       INTO V_RUBRICA_LEI1354
      FROM TB_CODIGO COD
         WHERE COD.COD_INS  = P_COD_INS   AND
               COD.COD_NUM  = 13542020    AND
               COD.COD_PAR  =  V_TIPO; 
      EXCEPTION
         WHEN OTHERS THEN
            V_RUBRICA_LEI1354:=2;
      END;
      
      RETURN V_RUBRICA_LEI1354;
 
 END; 
  FUNCTION SP_OBTEM_TETO_RGPS (P_DATA_OBITO DATE)    RETURN NUMBER AS
 
 V_TETO_RGPS      NUMBER:=0;
 V_DATA_OBITO     DATE;
 BEGIN
   
    IF P_DATA_OBITO  IS NULL THEN
        V_DATA_OBITO :=TRUNC(SYSDATE);
     ELSE
       V_DATA_OBITO := P_DATA_OBITO;
    END IF;
 
     BEGIN 
      SELECT   PA.VAL_ELEMENTO 
       INTO V_TETO_RGPS
      FROM TB_DET_PARAM_ESTRUTURA PA
         WHERE PA.COD_PARAM     =  'TASCO'      AND
               PA.COD_ESTRUTURA =   2000        AND
               PA.COD_ELEMENTO  =  'DESC_CONTR' AND
               PA.INI_VIG                     <=V_DATA_OBITO AND
               NVL(PA.FIM_VIG,V_DATA_OBITO)   >=V_DATA_OBITO; 
      EXCEPTION
         WHEN OTHERS THEN
            V_TETO_RGPS:=0;
      END;
      
      RETURN  V_TETO_RGPS;
 
 END SP_OBTEM_TETO_RGPS; 
 
 PROCEDURE  SP_GRAVA_RESERVA_COTA(P_COD_INS           IN  TB_NPM_BENEFICIARIO.COD_INS%TYPE
                                 ,P_COD_ADM_TRA       IN  TB_NPM_BENEFICIARIO.COD_ADM_TRA%TYPE 
                                 ,P_COD_IDE_CLI_SERV  IN  TB_NPM_BENEFICIARIO.COD_IDE_CLI_SERV%TYPE
                                 ,P_COD_BENEFICIO     IN  TB_BENEFICIARIO.COD_BENEFICIO%TYPE
                                 ,P_USU_PROC          IN  TB_BENEFICIO_RATEIO_PENSAO.NOM_USU_ULT_ATU%TYPE 
                                 ,P_COD_ERRO          OUT NUMBER
                                 ,P_MSG_ERRO          OUT VARCHAR2) 
   
 
 AS
   V_ROTINA VARCHAR2(40) := 'SP_GRAVA_RESERVA_COTA';
   V_COD_BENEFICIO  TB_BENEFICIARIO.COD_BENEFICIO%TYPE;
 BEGIN
   V_COD_BENEFICIO := NVL(P_COD_BENEFICIO, P_COD_ADM_TRA);
   -- Mergeia  Beneficiarios do ultimo registro do fluxo com tabela de reeserva 
   -- (aplica delete nos registros n?o existentes)  
   insert into TB_TMP_NPM_RESERVA_COTA
   (NUM_SEQ,
    COD_INS,
    COD_ADM_TRA,
    COD_IDE_CLI_SERV,
    COD_IDE_CLI_BEN,
    DAT_ING,
    DAT_ULT_ATU,
    NOM_USU_ULT_ATU,
    NOM_PRO_ULT_ATU,
    PROPORCAO,
    DAT_INI_BEN,
    DAT_INI_DEP,
    TMP_UNIAO_ESTAVEL_CASAMENTO,
    DAT_FIM_BEN_PREVISTA,
    FLG_INVALIDO_DEFICIENCIA,
    FLG_POSSUI_BEN_PREV,
    FLG_EXIGENCIA_PERICIA_MEDICA,
    FLG_EXIGENCIA_ACUMULAT_CUMPRIDA,
    FLG_EXIGENCIA_DOC_CUMPRIDA,
    COD_PERFIL_BENEFICIARIO,
    COD_CLASSE,
    COD_GRUPO,
    SUB_CLASSE,
    SEQ_REGRA_BENEF,
    EMBASAMENTO,
    FLG_DEFERIR,
    DESC_EXIGENCIA_PERICIA_MEDICA,
    DESC_EXIGENCIA_ACUMULAT_CUMPRIDA,
    DESC_EXIGENCIA_DOC_CUMPRIDA,
    FLG_REDUCAO_CUMULATIVIDADE,
    VAL_TOTAL_BENEFICIARIO,
    FLG_CAMINHO_EXIGENCIA_ACUMULATIVIDADE,
    FLG_DEVE_CRIAR_EXIG_PER_MED,
    FLG_DEVE_CRIAR_EXIG_DOC)
   SELECT  NUM_SEQ,
            COD_INS,
            COD_ADM_TRA,
            COD_IDE_CLI_SERV,
            COD_IDE_CLI_BEN,
            DAT_ING,
            DAT_ULT_ATU,
            NOM_USU_ULT_ATU,
            NOM_PRO_ULT_ATU,
            PROPORCAO,
            DAT_INI_BEN,
            DAT_INI_DEP,
            TMP_UNIAO_ESTAVEL_CASAMENTO,
            DAT_FIM_BEN_PREVISTA,
            FLG_INVALIDO_DEFICIENCIA,
            FLG_POSSUI_BEN_PREV,
            FLG_EXIGENCIA_PERICIA_MEDICA,
            FLG_EXIGENCIA_ACUMULAT_CUMPRIDA,
            FLG_EXIGENCIA_DOC_CUMPRIDA,
            COD_PERFIL_BENEFICIARIO,
            COD_CLASSE,
            COD_GRUPO,
            SUB_CLASSE,
            SEQ_REGRA_BENEF,
            EMBASAMENTO,
            FLG_DEFERIR,
            DESC_EXIGENCIA_PERICIA_MEDICA,
            DESC_EXIGENCIA_ACUMULAT_CUMPRIDA,
            DESC_EXIGENCIA_DOC_CUMPRIDA,
            FLG_REDUCAO_CUMULATIVIDADE,
            VAL_TOTAL_BENEFICIARIO,
            FLG_CAMINHO_EXIGENCIA_ACUMULATIVIDADE,
            FLG_DEVE_CRIAR_EXIG_PER_MED,
            FLG_DEVE_CRIAR_EXIG_DOC                  
            FROM(SELECT  NPM.*
                         ,MAX(COD_REGISTRO) OVER (PARTITION BY COD_INS, COD_IDE_CLI_SERV,COD_ADM_TRA) AS MAX_REG                    
                    FROM   TB_NPM_BENEFICIARIO NPM
                   WHERE  COD_INS          = P_COD_INS                 
                     AND  COD_ADM_TRA      = P_COD_ADM_TRA
                     AND  COD_IDE_CLI_SERV = P_COD_IDE_CLI_SERV) SUB
            WHERE SUB.COD_REGISTRO = MAX_REG
              AND (SUB.FLG_DEFERIR = 'S' OR
                   --(SUB.FLG_DEFERIR IS NULL AND 
                   -- TT69831 -- LJUNIOR EM 20/05/2021
                   (NVL(TRIM(SUB.FLG_DEFERIR),'N') = 'N' AND 
                    ((NVL(SUB.FLG_DEVE_CRIAR_EXIG_DOC,'N') = 'S') OR
                     (NVL(SUB.FLG_DEVE_CRIAR_EXIG_PER_MED,'N') = 'S') OR
                     (NVL(SUB.FLG_CAMINHO_EXIGENCIA_ACUMULATIVIDADE,'N') = 'S')  
                    )
                   )
                  )                     
              AND NOT EXISTS(SELECT '1'
                               FROM TB_BENEFICIARIO BEN
                              WHERE BEN.COD_INS = P_COD_INS
                                AND BEN.COD_IDE_CLI_BEN = SUB.COD_IDE_CLI_BEN
                                AND BEN.COD_BENEFICIO = V_COD_BENEFICIO );
    
  MERGE INTO TB_NPM_RESERVA_COTA CR
   USING TB_TMP_NPM_RESERVA_COTA A
      ON (A.COD_INS = CR.COD_INS AND 
          A.COD_ADM_TRA = CR.COD_ADM_TRA AND 
          V_COD_BENEFICIO = CR.COD_BENEFICIO AND
          A.COD_IDE_CLI_BEN = CR.COD_IDE_CLI_BEN)
   WHEN NOT MATCHED THEN 
      INSERT (NUM_SEQ,
              COD_INS,
              COD_ADM_TRA,
              COD_BENEFICIO, 
              COD_IDE_CLI_SERV,
              COD_IDE_CLI_BEN,
              DAT_ING,
              DAT_ULT_ATU,
              NOM_USU_ULT_ATU,
              NOM_PRO_ULT_ATU,
              PROPORCAO,
              DAT_INI_BEN,
              DAT_INI_DEP,
              TMP_UNIAO_ESTAVEL_CASAMENTO,
              DAT_FIM_BEN_PREVISTA,
              FLG_INVALIDO_DEFICIENCIA,
              FLG_POSSUI_BEN_PREV,
              FLG_EXIGENCIA_PERICIA_MEDICA,
              FLG_EXIGENCIA_ACUMULAT_CUMPRIDA,
              FLG_EXIGENCIA_DOC_CUMPRIDA,
              COD_PERFIL_BENEFICIARIO,
              COD_CLASSE,
              COD_GRUPO,
              SUB_CLASSE,
              SEQ_REGRA_BENEF,
              EMBASAMENTO,
              FLG_DEFERIR,
              DESC_EXIGENCIA_PERICIA_MEDICA,
              DESC_EXIGENCIA_ACUMULAT_CUMPRIDA,
              DESC_EXIGENCIA_DOC_CUMPRIDA,
              FLG_REDUCAO_CUMULATIVIDADE,
              VAL_TOTAL_BENEFICIARIO,
              FLG_CAMINHO_EXIGENCIA_ACUMULATIVIDADE,
              FLG_DEVE_CRIAR_EXIG_PER_MED,
              FLG_DEVE_CRIAR_EXIG_DOC,
              FLG_STATUS,
              DAT_INI_VIG,
              DAT_FIM_VIG)              
              VALUES
              (
              A.NUM_SEQ,
              A.COD_INS,
              A.COD_ADM_TRA,
              V_COD_BENEFICIO,
              A.COD_IDE_CLI_SERV,
              A.COD_IDE_CLI_BEN,
              SYSDATE,
              SYSDATE,
              P_USU_PROC,
              v_rotina,
              A.PROPORCAO,
              A.DAT_INI_BEN,
              A.DAT_INI_DEP,
              A.TMP_UNIAO_ESTAVEL_CASAMENTO,
              A.DAT_FIM_BEN_PREVISTA,
              A.FLG_INVALIDO_DEFICIENCIA,
              A.FLG_POSSUI_BEN_PREV,
              A.FLG_EXIGENCIA_PERICIA_MEDICA,
              A.FLG_EXIGENCIA_ACUMULAT_CUMPRIDA,
              A.FLG_EXIGENCIA_DOC_CUMPRIDA,
              A.COD_PERFIL_BENEFICIARIO,
              A.COD_CLASSE,
              A.COD_GRUPO,
              A.SUB_CLASSE,
              A.SEQ_REGRA_BENEF,
              A.EMBASAMENTO,
              A.FLG_DEFERIR,
              A.DESC_EXIGENCIA_PERICIA_MEDICA,
              A.DESC_EXIGENCIA_ACUMULAT_CUMPRIDA,
              A.DESC_EXIGENCIA_DOC_CUMPRIDA,
              A.FLG_REDUCAO_CUMULATIVIDADE,
              A.VAL_TOTAL_BENEFICIARIO,
              A.FLG_CAMINHO_EXIGENCIA_ACUMULATIVIDADE,
              A.FLG_DEVE_CRIAR_EXIG_PER_MED,
              A.FLG_DEVE_CRIAR_EXIG_DOC,
              'V',
              SYSDATE,
              NULL
              ) 
   WHEN MATCHED THEN
     UPDATE SET DAT_ULT_ATU = sysdate,
                NOM_USU_ULT_ATU = P_USU_PROC,
                NOM_PRO_ULT_ATU = v_rotina,
                PROPORCAO = A.PROPORCAO,
                DAT_INI_BEN = A.DAT_INI_BEN,
                DAT_INI_DEP = A.DAT_INI_DEP,
                TMP_UNIAO_ESTAVEL_CASAMENTO = A.TMP_UNIAO_ESTAVEL_CASAMENTO,
                DAT_FIM_BEN_PREVISTA = A.DAT_FIM_BEN_PREVISTA,
                FLG_INVALIDO_DEFICIENCIA = A.FLG_INVALIDO_DEFICIENCIA,
                FLG_POSSUI_BEN_PREV = A.FLG_POSSUI_BEN_PREV,
                FLG_EXIGENCIA_PERICIA_MEDICA = A.FLG_EXIGENCIA_PERICIA_MEDICA,
                FLG_EXIGENCIA_ACUMULAT_CUMPRIDA = A.FLG_EXIGENCIA_ACUMULAT_CUMPRIDA,
                FLG_EXIGENCIA_DOC_CUMPRIDA = A.FLG_EXIGENCIA_DOC_CUMPRIDA,
                COD_PERFIL_BENEFICIARIO = A.COD_PERFIL_BENEFICIARIO,
                COD_CLASSE = A.COD_CLASSE,
                COD_GRUPO = A.COD_GRUPO,
                SUB_CLASSE = A.SUB_CLASSE,
                SEQ_REGRA_BENEF = A.SEQ_REGRA_BENEF,
                EMBASAMENTO = A.EMBASAMENTO,
                FLG_DEFERIR = A.FLG_DEFERIR,
                DESC_EXIGENCIA_PERICIA_MEDICA = A.DESC_EXIGENCIA_PERICIA_MEDICA,
                DESC_EXIGENCIA_ACUMULAT_CUMPRIDA = A.DESC_EXIGENCIA_ACUMULAT_CUMPRIDA,
                DESC_EXIGENCIA_DOC_CUMPRIDA = A.DESC_EXIGENCIA_DOC_CUMPRIDA,
                FLG_REDUCAO_CUMULATIVIDADE = A.FLG_REDUCAO_CUMULATIVIDADE,
                VAL_TOTAL_BENEFICIARIO = A.VAL_TOTAL_BENEFICIARIO,
                FLG_CAMINHO_EXIGENCIA_ACUMULATIVIDADE = A.FLG_CAMINHO_EXIGENCIA_ACUMULATIVIDADE,
                FLG_DEVE_CRIAR_EXIG_PER_MED = A.FLG_DEVE_CRIAR_EXIG_PER_MED,
                FLG_DEVE_CRIAR_EXIG_DOC = A.FLG_DEVE_CRIAR_EXIG_DOC;   
 
   UPDATE TB_NPM_RESERVA_COTA RC
      SET FLG_STATUS = 'C',
          DAT_FIM_VIG = SYSDATE
    WHERE COD_INS          = P_COD_INS                 
      AND COD_ADM_TRA      = P_COD_ADM_TRA 
      AND COD_IDE_CLI_SERV = P_COD_IDE_CLI_SERV 
      AND FLG_STATUS = 'V' 
      AND COD_IDE_CLI_BEN NOT IN(SELECT COD_IDE_CLI_BEN 
                                   FROM TB_TMP_NPM_RESERVA_COTA);
      
   COMMIT;
 EXCEPTION
   WHEN OTHERS THEN
     P_COD_ERRO := SQLCODE;
     P_MSG_ERRO := SQLERRM;
         
     ROLLBACK;        

 END SP_GRAVA_RESERVA_COTA;
 --
 --
 PROCEDURE SP_INCL_IMPORTA_CALCULO (P_COD_INS           IN  TB_NPM_BENEFICIARIO.COD_INS%TYPE
                                   ,P_COD_ADM_TRA       IN  TB_NPM_BENEFICIARIO.COD_ADM_TRA%TYPE 
                                   ,P_USU_PROC          IN  TB_BENEFICIO_RATEIO_PENSAO.NOM_USU_ULT_ATU%TYPE 
                                   ,P_COD_ERRO          OUT NUMBER
                                   ,P_MSG_ERRO          OUT VARCHAR2) 
 AS
   V_COD_ADM_TRA_ASSOC   TB_NPM_BENEFICIARIO.COD_ADM_TRA%TYPE;
   v_qtd number;
   v_qtd_2 number;
 BEGIN
   
   -- 
   SELECT Cli_Cod_Adm_Tra_Assoc
     INTO V_COD_ADM_TRA_ASSOC
     FROM WRKCLI CLI
    WHERE CLI.CLI_COD_INS = P_COD_INS
      AND CLI.CLI_COD_ADM_TRA = P_COD_ADM_TRA;      
   --
   --
   select count(1) 
     into v_qtd_2
     from tb_comp_sal_tra cst
    where cst.cod_ins = 1
      and cst.cod_adm_tra = V_COD_ADM_TRA_ASSOC; 
   --                  
   IF v_qtd_2 > 0 THEN               
     delete tb_comp_sal_tra
      where cod_ins = 1
        and cod_adm_tra = P_COD_ADM_TRA;   
     --
     insert into tb_comp_sal_tra cst
     (cod_ins,
      cod_ide_cli,
      num_matricula,
      cod_ide_rel_func,
      cod_rubrica,
      cod_fcrubrica,
      cod_entidade,
      dat_mes,
      dat_ano,
      num_seq_vig,
      val_rubrica,
      flg_participa_con,
      flg_integra_sal,
      val_por,
      cod_ref_pad_venc,
      des_info,
      cod_adm_tra,
      dat_ing,
      dat_ult_atu,
      nom_usu_ult_atu,
      nom_pro_ult_atu,
      val_unidade,
      val_prop_tram,
      num_seq,
      cod_referencia_2,
      cod_tabela,
      cod_funcao,
      cod_cargo_incorp,
      dat_incorp,
      cod_cargo,
      num_refe_calc,
      flg_editavel,
      dat_incorp_rub,
      cod_cargo_2)
      select 
      cod_ins,
      cod_ide_cli,
      num_matricula,
      cod_ide_rel_func,
      cod_rubrica,
      cod_fcrubrica,
      cod_entidade,
      dat_mes,
      dat_ano,
      num_seq_vig,
      val_rubrica,
      flg_participa_con,
      flg_integra_sal,
      val_por,
      cod_ref_pad_venc,
      des_info,
      P_COD_ADM_TRA, -- cod_adm_tra
      sysdate,-- dat_ing
      sysdate, --dat_ult_atu,
      user, --nom_usu_ult_atu,
      P_USU_PROC, --nom_pro_ult_atu,
      val_unidade,
      val_prop_tram,
      num_seq,
      cod_referencia_2,
      cod_tabela,
      cod_funcao,
      cod_cargo_incorp,
      dat_incorp,
      cod_cargo,
      num_refe_calc,
      flg_editavel,
      dat_incorp_rub,
      cod_cargo_2
      from tb_comp_sal_tra cst
      where cst.cod_ins = 1
        and cst.cod_adm_tra = V_COD_ADM_TRA_ASSOC;     
  END IF;           
 
  EXCEPTION
   WHEN OTHERS THEN
     P_COD_ERRO := SQLCODE;
     P_MSG_ERRO := SQLERRM;
         
     ROLLBACK;        

 END SP_INCL_IMPORTA_CALCULO;
   
 
 
 END PAC_CALCULA_PENSAO_NPM;
/
