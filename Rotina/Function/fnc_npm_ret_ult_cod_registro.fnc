create or replace function user_ipesp.fnc_npm_ret_ult_cod_registro(i_cod_ins       in wrkreg.reg_cod_ins%type,
                                                        i_cod_adm_tra   in wrkreg.reg_cod_adm_tra%type,
                                                        i_cod_registro  in wrkreg.reg_cod_registro%type
                                                        ) return number
as
  v_qtd          number;
  v_cod_registro wrkreg.reg_cod_registro%type;
begin
    -- localizando registro valido
    select count(1)
      into v_qtd
      from tb_npm_beneficiario n2
     where n2.cod_ins = i_cod_ins
       and n2.cod_adm_tra = i_cod_adm_tra
       and n2.cod_registro = i_cod_registro;
       
    if v_qtd = 0 then
      begin
        select cod_registro
          into v_cod_registro
          from tb_npm_beneficiario n2
         where n2.cod_ins = i_cod_ins
           and n2.cod_adm_tra = i_cod_adm_tra
           and n2.num_seq = (select max(num_Seq)
                               from tb_npm_beneficiario n2
                              where n2.cod_ins = i_cod_ins
                                and n2.cod_adm_tra = i_cod_adm_tra);
      exception
        when no_data_found then
          v_cod_registro := null;
      end;
    else
      v_cod_registro := i_cod_registro;
    end if;
    
    return v_cod_registro;
end fnc_npm_ret_ult_cod_registro;
/
