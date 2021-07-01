create or replace package user_ipesp.pac_wrk_intersec
as
   function fnc_bloqueia_formalizacao(i_cod_ins            in wrktip.tip_cod_ins%type,
                                      i_cod_adm_tra        in wrkcli.cli_cod_adm_tra%type,
                                      i_cod_reg            in wrkreg.reg_cod_registro%type,    
                                      i_usuario            in varchar2,
                                      i_processo           in varchar2) return char; -- N ou S
    
   procedure sp_sincroniza_dados_tarefa(i_cod_ins            in wrktip.tip_cod_ins%type,                              
                                       i_cod_adm_tra        in wrkcli.cli_cod_adm_tra%type,
                                       i_cod_adm_tra_assoc  in wrkcli.cli_cod_adm_tra%type,
                                       o_msg_status         out varchar2,
                                       o_cod_status         out number,
                                       i_usuario            in varchar2,
                                       i_processo           in varchar2);                                  
   procedure sp_wrk_intersec(i_cod_ins            in wrktip.tip_cod_ins%type,
                             i_cod_adm_tra        in wrkcli.cli_cod_adm_tra%type,                           
                             i_cod_reg            in wrkreg.reg_cod_registro%type,
                             i_opc                in wrkreg.reg_cod_opc%TYPE,
                             o_cod_status         out number,   -- =0: normal, sem intersecção >0 com intersecção <0 erro
                             o_msg_status         out varchar2,
                             i_usuario            in varchar2,
                             i_processo           in varchar2);

end pac_wrk_intersec;
/
create or replace package body user_ipesp.pac_wrk_intersec
as

  
 function fnc_bloqueia_formalizacao(i_cod_ins            in wrktip.tip_cod_ins%type,
                                    i_cod_adm_tra        in wrkcli.cli_cod_adm_tra%type,
                                    i_cod_reg            in wrkreg.reg_cod_registro%type,                                  
                                    i_usuario            in varchar2,
                                    i_processo           in varchar2) return char
  as
    v_qtd number;
  begin 
    select count(1)
      into v_qtd
      from wrkcli cli join wrkreg reg on cli.cli_cod_ins = reg.reg_cod_ins
                                      and cli.cli_cod_adm_tra = reg.reg_cod_adm_tra
     where cli.cli_cod_ins = i_cod_ins
       and cli.cli_cod_adm_tra_assoc = i_cod_adm_tra
       and reg.reg_cod_tarefa = 301
       and reg.reg_cod_estado = 'P';
    if v_qtd > 0 then    
     return 'S';
    else
     return 'N';
    end if;
  end fnc_bloqueia_formalizacao;   
  
  

 function fnc_ret_opc(i_cod_ins           in char, 
                      i_cod_adm_tra       in varchar2,
                      i_cod_tipo          in varchar2,
                      i_cod_tar           in varchar2
                      ) return number
 as
   v_cod_opc number;
   v_cmd     varchar2(4000);
 begin 
   --
   v_cmd := 'select reg_cod_opc
              from (select reg.reg_cod_opc
                      from wrkreg reg
                     where reg_cod_ins = :1  
                       and reg_cod_tipo = :2
                       and reg_cod_adm_tra = :3 
                       and reg_cod_tarefa in ('||i_cod_tar||')
                       and reg.reg_cod_estado <> ''E''
                       and reg.reg_cod_opc is not null
                       and reg_fec_ini = (select max(reg_fec_ini)
                                            from wrkreg reg2
                                           where reg2.reg_cod_ins = reg.reg_cod_ins   
                                             and reg2.reg_cod_tipo = reg.reg_cod_tipo
                                             and reg2.reg_cod_adm_tra = reg.reg_cod_adm_tra
                                             and reg2.reg_cod_tarefa in ('||i_cod_tar||')
                                             and reg.reg_cod_estado <> ''E'' 
                                             and reg.reg_cod_opc is not null))';
   
   dbms_output.put_line(v_cmd);
   begin
     execute immediate v_cmd into  v_cod_opc using i_cod_ins, i_cod_tipo, i_cod_adm_tra;
   exception 
      when no_data_found then 
        v_cod_opc := null;
   end;
   --  
   return v_cod_opc;                       
 end fnc_ret_opc; 
  --                       
  function fnc_valida_condicao(i_cod_ins           in char, 
                               i_cod_tipo          in varchar2,
                               i_cod_tarefa        in varchar2,
                               i_cod_adm_tra       in varchar2,
                               i_cod_adm_tra_assoc in varchar2
                               ) return char
  as
    v_cmd varchar2(4000);
    v_qtd number;
    v_Res char;
    v_cod_adm_Tra varchar2(20);
    v_cod_tipo    varchar2(20);
  begin 
    -- Verificando critérios 
    for r in
    (select num_seq_regras_intersec
       from tb_wrk_regras_intersec r
      where r.cod_ins = i_cod_ins
        and r.cod_tipo_fluxo = i_cod_tipo
        and r.cod_tarefa = i_cod_tarefa
        and r.num_seq_regras_intersec_pai is null
     )
    loop         
      --
      for i in (
      select cod_tipo_fluxo, 
             cod_tipo_fluxo_assoc,
             des_lista_status,
             cod_tarefa,
             des_lista_tarefas_assoc,
             cod_condicao,
             flg_tipo_fluxo_opc,
             des_lista_tarefas_opc,
             val_tarefa_opc                          
        from tb_wrk_regras_intersec wrk
       where cod_ins = '1' 
       connect by prior wrk.num_seq_regras_intersec = wrk.num_seq_regras_intersec_pai
       start with wrk.num_seq_regras_intersec = r.num_seq_regras_intersec
      )
      loop
        if i.val_tarefa_opc is not null then                   
          if i.flg_tipo_fluxo_opc = 'A' then --associado
            v_cod_adm_tra := i_cod_adm_tra_assoc;
            v_cod_tipo    := i.cod_tipo_fluxo_assoc;
          else
            v_cod_adm_tra := i_cod_adm_tra;
            v_cod_tipo    := i.cod_tipo_fluxo;            
          end if;  
          --     
          --               
          v_res := fnc_ret_opc(i_cod_ins,
                               v_cod_adm_tra,                      
                               v_cod_tipo,      
                               i.des_lista_tarefas_opc);   
                               
          if v_res <> i.val_tarefa_opc then 
            continue;
          end if;
        end if;
        
        v_cmd := 'select count(1)
                    from (select reg.reg_cod_estado
                            from wrkreg reg
                           where reg_cod_ins = :1  
                             and reg_cod_tipo = :2
                             and reg_cod_adm_tra = :3 
                             and reg_cod_tarefa in ('||i.des_lista_tarefas_assoc||')
                             and reg.reg_cod_estado <> ''E''
                             and reg_fec_ini = (select max(reg_fec_ini)
                                                  from wrkreg reg2
                                                 where reg2.reg_cod_ins = reg.reg_cod_ins   
                                                   and reg2.reg_cod_tipo = reg.reg_cod_tipo
                                                   and reg2.reg_cod_adm_tra = reg.reg_cod_adm_tra
                                                   and reg2.reg_cod_tarefa in ('||i.des_lista_tarefas_assoc||')
                                                   and reg.reg_cod_estado <> ''E'')
                                                   )';
        --
        --
        if i.des_lista_status is not null then               
          v_cmd :=  v_cmd||' where reg_cod_estado in ('||i.des_lista_status||')';
        end if;   
        dbms_output.put_line(v_cmd);
        --
        execute immediate v_cmd into  v_qtd
                                using i_cod_ins,
                                      i.cod_tipo_fluxo_assoc,
                                      i_cod_adm_tra_assoc;
        --                                            
        if v_qtd > 0 and i.cod_condicao = 'E' or 
           v_qtd = 0 and i.cod_condicao = 'N' then 
          v_Res:= 'S';
        else
          v_Res:= 'N'; -- condicao and (se uma linha da condicao falhar, falhou a condicao pai geral)
          exit;         
        end if;  
      end loop;
      --
      if v_Res = 'S' then -- condicao or (se uma linha da condicao com suceso, resultado deve ser positivo)
        exit;
      end if;                                                                                                                                                                        
    end loop;  
    --
    --
    return v_Res;  
    --
    --
  end fnc_valida_condicao;
  --
  --
  procedure sp_finaliza_tarefa(i_cod_ins            in wrktip.tip_cod_ins%type,                                                              
                               i_cod_adm_tra        in wrkcli.cli_cod_adm_tra%type,
                               i_cod_tipo           in wrkreg.reg_cod_tipo%type,
                               i_cod_ide_cli        in wrkreg.reg_ide_cli%type,
                               i_cod_tar_final      in wrkreg.reg_cod_tarefa%type,
                               o_des_Erro           out varchar2,
                               o_cod_erro           out number,
                               i_usuario            in varchar2,
                               i_processo           in varchar2)
  as
    v_qtd     number;
    --
    v_cod_erro number;
    v_des_erro varchar2(4000);
    v_cod_registro number;
    --
    e_erro     exception;
    --
    v_des_perfil wrkreg.reg_cod_perfil%type;
  begin 
    -- 
    --
    update wrkreg reg
       set reg.reg_cod_estado = 'E',
           reg.reg_fec_termino = sysdate,
           reg.reg_fec_ult_man = sysdate,
           reg.reg_nom_usu_man = i_usuario,
           reg.reg_nom_pro_man = i_processo
     where reg.reg_cod_ins = i_cod_ins  
       and reg.reg_cod_adm_tra = i_cod_adm_tra
       and reg.reg_cod_estado = 'I';
    --
    update wrkreg reg
       set reg.reg_cod_estado = 'T',
           reg.reg_fec_termino = sysdate,
           reg.reg_fec_ult_man = sysdate,
           reg.reg_nom_usu_man = i_usuario,
           reg.reg_nom_pro_man = i_processo
     where reg.reg_cod_ins = i_cod_ins  
       and reg.reg_cod_adm_tra = i_cod_adm_tra
       and reg.reg_cod_estado = 'P';       
    --
    -- perfis
    select tar_cod_perfil
      into v_des_perfil
      from wrktar tar 
     where tar.tar_cod_ins = i_cod_ins
       and tar.tar_cod_tipo = i_cod_tipo
       and tar.tar_cod_tarefa = i_cod_tar_final;
    --      
    -- inserindo tarefa 
    select count(1)
      into v_qtd
      from wrkreg reg
     where reg.reg_cod_ins = i_cod_ins  
       and reg.reg_cod_adm_tra = i_cod_adm_tra
       and reg.reg_cod_tarefa = i_cod_tar_final
       and reg.reg_cod_estado <> 'E';
    
    if v_qtd = 0 then 
      v_cod_registro  := seq_wrk_reg.nextval;
      insert into wrkreg
      (REG_COD_REGISTRO,
       REG_COD_ETAPA,
       REG_COD_TAREFA,
       REG_COD_ESTADO,
       REG_DES_OBS,
       REG_COD_USUARIO,
       REG_COD_INS,
       REG_IDE_CLI,
       REG_COD_TIPO,
       REG_COD_ADM_TRA,
       REG_IDE_CODNUM,
       REG_COD_PERFIL,
       REG_FEC_INI,
       REG_FEC_TERMINO,
       REG_FEC_ING,
       REG_FEC_ULT_MAN,
       REG_NOM_USU_MAN,
       REG_NOM_PRO_MAN,
       REG_COD_OPC,
       REG_DOC_DIG_ESTADO,
       REG_COD_PERFIL_FORMALIZADO,
       REG_COD_ADM_TRA_ASSOC)
       values
       (v_cod_registro,       --REG_COD_REGISTRO,
        1,                    --REG_COD_ETAPA,
        i_cod_tar_final,      --REG_COD_TAREFA,
        'P',                  --REG_COD_ESTADO,
        null,                 --REG_DES_OBS,
        i_usuario,            --REG_COD_USUARIO,
        i_cod_ins,            --REG_COD_INS,
        i_cod_ide_cli,        --REG_IDE_CLI,
        i_cod_tipo,           --REG_COD_TIPO,
        i_cod_adm_tra,        --REG_COD_ADM_TRA,
        null,                 --REG_IDE_CODNUM,
        v_des_perfil,         --REG_COD_PERFIL,
        sysdate,              --REG_FEC_INI,
        null,                 --REG_FEC_TERMINO,
        sysdate,              --REG_FEC_ING,
        sysdate,              --REG_FEC_ULT_MAN,
        i_usuario,            --REG_NOM_USU_MAN,
        i_processo,           --REG_NOM_PRO_MAN,
        null,                 --REG_COD_OPC,
        null,                 --REG_DOC_DIG_ESTADO,
        null,                 --REG_COD_PERFIL_FORMALIZADO,
        null                  --REG_COD_ADM_TRA_ASSOC               
       );
       --
      select count(1)
        into v_qtd
        from wrkacd acd
       where acd.acd_cod_ins = i_cod_ins  
         and acd.acd_cod_registro = v_cod_registro
         and acd.acd_cod_tarefa = i_cod_tar_final ;
       --
       if v_qtd = 0 then
         insert into wrkacd
         (acd_cod_registro,
          acd_cod_etapa,
          acd_cod_tarefa,
          acd_cod_ins,
          acd_cod_tipo,
          acd_cod_perfil,
          acd_cod_atividade,
          acd_status_ativ,
          acd_fec_ing,
          acd_fec_ult_man,
          acd_nom_usu_man,
          acd_nom_pro_man)
          values
          (v_cod_registro,
           1,
           i_cod_tar_final,
           i_cod_ins,
           i_cod_tipo,
           v_des_perfil,
           'ACT0000105',
           'N',
           sysdate,
           sysdate,
           i_usuario,
           i_processo);
         end if;

     end if;

  exception 
    when e_erro then             
      rollback;
      o_cod_erro := 2;
      o_Des_erro := v_des_erro;      
    when others then  
      v_des_erro := sqlcode||'-'||sqlerrm;
      rollback;
      o_cod_erro := 1;
      o_Des_erro := v_des_erro;                  
  end sp_finaliza_tarefa;                               
  --
  --
  procedure sp_sincroniza_dados_tarefa(i_cod_ins            in wrktip.tip_cod_ins%type,                              
                                       i_cod_adm_tra        in wrkcli.cli_cod_adm_tra%type,
                                       i_cod_adm_tra_assoc  in wrkcli.cli_cod_adm_tra%type,
                                       o_msg_status         out varchar2,
                                       o_cod_status         out number,
                                       i_usuario            in varchar2,
                                       i_processo           in varchar2)
  as
    v_qtd number;      
    v_num_seq number;
    v_des_erro varchar2(4000);
    
  begin 
    -- Se ja tem flag , indica que ja foi incluido
    select count(1)
      into v_qtd
      from tb_npm_beneficiario npm
     where npm.cod_ins = i_cod_ins
       and npm.cod_adm_tra = i_cod_adm_tra_assoc
       and npm.flg_dep_inclusao = 'S';
    -- 
    if v_qtd > 0 then 
      return;
    end if;           
    --
    v_num_seq := seq_npm_beneficiario.nextval;
    --   
    insert into tb_npm_beneficiario 
    (num_seq,
     cod_ins,
     cod_adm_tra,
     cod_tarefa,
     cod_registro,
     cod_ide_cli_serv,
     cod_ide_cli_ben,
     dat_ing,
     dat_ult_atu,
     nom_usu_ult_atu,
     nom_pro_ult_atu,
     proporcao,
     dat_ini_ben,
     dat_ini_dep,
     tmp_uniao_estavel_casamento,
     dat_fim_ben_prevista,
     flg_invalido_deficiencia,
     flg_possui_ben_prev,
     flg_exigencia_pericia_medica,
     flg_exigencia_acumulat_cumprida,
     flg_exigencia_doc_cumprida,
     cod_perfil_beneficiario,
     cod_classe,
     cod_grupo,
     sub_classe,
     seq_regra_benef,
     embasamento,
     flg_deferir,
     desc_exigencia_pericia_medica,
     desc_exigencia_acumulat_cumprida,
     desc_exigencia_doc_cumprida,
     flg_reducao_cumulatividade,
     val_total_beneficiario,
     flg_caminho_exigencia_acumulatividade,
     flg_deve_criar_exig_per_med,
     flg_deve_criar_exig_doc,
     num_depend,
     dat_nasc,
     cod_parentesco,
     observacoes,
     dat_requerimento,
     flg_dep_inclusao)
     select 
       v_num_seq as num_seq,
       cod_ins,
       i_cod_adm_tra_assoc as cod_adm_tra,
       cod_tarefa,
       cod_registro,
       cod_ide_cli_serv,
       cod_ide_cli_ben,
       dat_ing,
       dat_ult_atu,
       nom_usu_ult_atu,
       nom_pro_ult_atu,
       proporcao,
       dat_ini_ben,
       dat_ini_dep,
       tmp_uniao_estavel_casamento,
       dat_fim_ben_prevista,
       flg_invalido_deficiencia,
       flg_possui_ben_prev,
       flg_exigencia_pericia_medica,
       flg_exigencia_acumulat_cumprida,
       flg_exigencia_doc_cumprida,
       cod_perfil_beneficiario,
       cod_classe,
       cod_grupo,
       sub_classe,
       seq_regra_benef,
       embasamento,
       flg_deferir,
       desc_exigencia_pericia_medica,
       desc_exigencia_acumulat_cumprida,
       desc_exigencia_doc_cumprida,
       flg_reducao_cumulatividade,
       val_total_beneficiario,
       flg_caminho_exigencia_acumulatividade,
       flg_deve_criar_exig_per_med,
       flg_deve_criar_exig_doc,
       num_depend,
       dat_nasc,
       cod_parentesco,
       observacoes,
       dat_requerimento,
       flg_dep_inclusao
     from tb_npm_beneficiario nbpm
    where nbpm.cod_ins = i_cod_ins
      and nbpm.cod_adm_tra = i_cod_adm_tra
      and nbpm.flg_dep_inclusao = 'S';
    --
    if sql%rowcount = 0 then 
      insert into benipa  
             (IPA_COD_INS,
              IPA_IDE_CLI,
              IPA_IND_TIP_TRA,
              IPA_COD_ADM_TRA,
              IPA_NUM_BEN,
              IPA_IND_VIG_PAG,
              IPA_FEC_VIG_PAG,
              IPA_IDE_REC_PAG,
              IPA_FEC_DEV,
              IPA_FEC_CES,
              IPA_IND_CAU_CES,
              IPA_GRU_PRO,
              IPA_FEC_PRO,
              IPA_FEC_ULT_LIQ,
              IPA_PER_ULT_DIS,
              IPA_PER_PRI_DIS,
              IPA_FEC_ING,
              IPA_FEC_ULT_MAN,
              IPA_NOM_USU_MAN,
              IPA_NOM_PRO_MAN,
              IPA_PORC_BEN,
              IPA_GRP_FAM,
              IPA_TIP_REP,
              IPA_COD_CON,
              IPA_IDE_BEN,
              IPA_SEQ_REGRA_BENEF,
              IPA_COD_CLASSE,
              IPA_COD_GRUPO,
              IPA_COD_PERFIL,
              IPA_FLG_STATUS,
              IPA_ENQUAD_PAG,
              IPA_OBS,
              IPA_SUBCLASSE,
              IPA_NUM_ORD_JUD,
              IPA_COD_TIP_CAP,
              IPA_COD_PARENTESCO,
              IPA_FLG_INVALIDO_DEFICIENCIA)   
       select IPA_COD_INS,
              IPA_IDE_CLI,
              IPA_IND_TIP_TRA,
              i_cod_adm_tra_assoc as IPA_COD_ADM_TRA,
              IPA_NUM_BEN,
              IPA_IND_VIG_PAG,
              IPA_FEC_VIG_PAG,
              IPA_IDE_REC_PAG,
              IPA_FEC_DEV,
              IPA_FEC_CES,
              IPA_IND_CAU_CES,
              IPA_GRU_PRO,
              IPA_FEC_PRO,
              IPA_FEC_ULT_LIQ,
              IPA_PER_ULT_DIS,
              IPA_PER_PRI_DIS,
              IPA_FEC_ING,
              IPA_FEC_ULT_MAN,
              IPA_NOM_USU_MAN,
              IPA_NOM_PRO_MAN,
              IPA_PORC_BEN,
              IPA_GRP_FAM,
              IPA_TIP_REP,
              IPA_COD_CON,
              IPA_IDE_BEN,
              IPA_SEQ_REGRA_BENEF,
              IPA_COD_CLASSE,
              IPA_COD_GRUPO,
              IPA_COD_PERFIL,
              IPA_FLG_STATUS,
              IPA_ENQUAD_PAG,
              IPA_OBS,
              IPA_SUBCLASSE,
              IPA_NUM_ORD_JUD,
              IPA_COD_TIP_CAP,
              IPA_COD_PARENTESCO,
              IPA_FLG_INVALIDO_DEFICIENCIA
       from benipa ben
      where ben.ipa_cod_ins = i_cod_ins
        and ben.ipa_cod_adm_tra = i_cod_adm_tra;    
    end if;  
    --
    o_cod_status := 0;
  exception    
    when others then  
      v_des_erro := sqlcode||'-'||sqlerrm;
      o_cod_status := 1;
      o_msg_status := v_des_erro; 
  end sp_sincroniza_dados_tarefa;
  -- 
  -- 
  procedure sp_wrk_intersec(i_cod_ins            in wrktip.tip_cod_ins%type,
                            i_cod_adm_tra        in wrkcli.cli_cod_adm_tra%type,                           
                            i_cod_reg            in wrkreg.reg_cod_registro%type,
                            i_opc                in wrkreg.reg_cod_opc%TYPE,
                            o_cod_status         out number,   -- =0: normal, sem intersecção >0 com intersecção <0 erro
                            o_msg_status         out varchar2,
                            i_usuario            in varchar2,
                            i_processo           in varchar2)
  as
    e_erro     exception;
    v_cod_erro number;
    v_des_erro varchar2(4000);  
    v_qtd      number;
    --
    v_cod_num        number := 20014;
    v_cod_num_final  number := 20015;
    v_cod_par        varchar2(40);
    --        
    v_cod_tipo            wrkcli.cli_ind_tip_tra%type;
    v_cod_adm_tra_assoc   wrkcli.cli_cod_adm_tra%type;
    v_cod_tar             wrkreg.reg_cod_tarefa%type;
    v_cod_tar_final       wrkreg.reg_cod_tarefa%type; 
    v_cod_ide_cli         wrkcli.cli_ide_cli%type;
    v_cli_est_act_tra     wrkcli.cli_est_act_tra%type;
     
  begin 
    
    --
    --    
    begin 
      select reg.reg_cod_tipo,
             reg.reg_cod_tarefa    
        into v_cod_tipo,
             v_cod_tar
        from wrkreg reg
       where reg.reg_cod_ins = i_cod_ins    
         and reg.reg_cod_adm_tra = i_cod_adm_tra
         and reg.reg_cod_registro = i_cod_reg;
         
    exception 
      when no_data_found then 
        v_cod_tipo := null;  
    end;  
    --
    if v_cod_tipo is null then 
      v_des_erro := 'Nâo localizado tarefa fluxo referente ao registro especificado';
      raise e_erro;
    end if; 
     -- Verifica se fluxo e tarefa fazem parte da lista de instersecção
    begin 
      select cod.cod_par
        into v_cod_par
        from tb_codigo cod
       where cod_num = v_cod_num
         and cod_par = v_cod_tipo||'/'||to_char(v_cod_tar);
    exception 
      when no_data_found then 
        o_cod_status := 0;
        return;
    end;         
    -- Pega valor da tarefa de finalização e exclui se existir no fluxo com 'I'
    -- Seleciona a tarefa de finalização
    begin 
      select des_descricao
        into v_cod_tar_final
        from tb_codigo c
       where c.cod_num = v_cod_num_final
         and c.cod_par = v_cod_tipo;
    exception 
      when no_data_found then 
        v_des_erro := 'Não localizada parametrização da tarefa de finalização';
        raise e_erro;
    end;         
    --
    update wrkreg reg
       set reg.reg_cod_estado = 'E'
     where reg.reg_cod_ins = i_cod_ins    
       and reg.reg_cod_adm_tra = i_cod_adm_tra
       and reg.reg_cod_tarefa = v_cod_tar_final
       and reg.reg_cod_estado = 'I';          
    --
    commit; -- atualizacao da tarefa de finalização deve ser efetivada sempre
    --
    UPDATE wrkreg REG
       SET REG.REG_COD_OPC = i_opc
     WHERE  reg.reg_cod_ins = i_cod_ins    
       AND REG.REG_COD_ADM_TRA = i_cod_adm_tra
       AND REG.REG_COD_REGISTRO = i_cod_reg;          
    --         
    --   
    --
    -- Valida parametros de entrada
    select cli.cli_ide_cli,
           cli.cli_cod_adm_tra_assoc,
           cli.cli_est_act_tra           
      into v_cod_ide_cli,
           v_cod_adm_tra_assoc,
           v_cli_est_act_tra
      from wrkcli cli
     where cli.cli_cod_ins = i_cod_ins    
       and cli.cli_cod_adm_tra = i_cod_adm_tra;
    --
    -- 
    if v_cod_ide_cli is null then 
      v_des_erro := 'Nâo localizado fluxo referente ao protocolo especificado';
      raise e_erro;
    end if;   
    --
    if v_cli_est_act_tra = 'H' or 
       v_cod_adm_tra_assoc is null then 
       o_cod_status := 0;
    end if;            
    --
    --
    if o_cod_status is null then 
      if fnc_valida_condicao(i_cod_ins,
                             v_cod_tipo,
                             v_cod_tar,
                             i_cod_adm_tra,
                             v_cod_adm_tra_assoc
                             ) = 'S' then 
         --       
         --             
         sp_finaliza_tarefa(i_cod_ins,                                                          
                            i_cod_adm_tra,
                            v_cod_tipo,                          
                            v_cod_ide_cli,
                            v_cod_tar_final,
                            v_des_erro,
                            v_cod_erro,
                            i_usuario,
                            i_processo);  
         --                           
         if v_cod_erro <> 0 then 
           v_des_erro := v_des_erro;
           raise e_erro;     
         end if;                          
         --
         sp_sincroniza_dados_tarefa(i_cod_ins, 
                                    i_cod_adm_tra, 
                                    v_cod_adm_tra_assoc,                                   
                                    v_cod_erro, 
                                    v_des_erro,
                                    i_usuario,
                                    i_processo                                  
                                    );                          
         --                      
         if v_cod_erro <> 0 then 
           v_des_erro := v_des_erro;
           raise e_erro;     
         end if;         
         
         v_cod_erro := 1;                                  
      else
        v_cod_erro := 0;  
      end if;   
      --
      o_cod_status := v_cod_erro;           
    end if;
    --
    if o_cod_status = 0  then 
      rollback;
    end if;
    --
  exception 
    when e_erro then 
      rollback;
      o_cod_status := -1;   
      o_msg_status :=  v_des_erro;    
    when others then 
      o_msg_status := sqlcode||'-'||sqlerrm;
      o_cod_status := -2;            
      rollback;
  end sp_wrk_intersec;

end pac_wrk_intersec;
/
