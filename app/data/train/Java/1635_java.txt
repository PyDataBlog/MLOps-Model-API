/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package br.com.stylehair.servicos;

import br.com.stylehair.dao.AgendamentoDAO;
import br.com.stylehair.entity.Agendamento;
import com.google.gson.Gson;
import java.text.SimpleDateFormat;
import java.util.Date;
import java.util.List;
import javax.ejb.Stateless;
import javax.persistence.EntityManager;
import javax.persistence.PersistenceContext;
import javax.ws.rs.Consumes;
import javax.ws.rs.GET;
import javax.ws.rs.POST;
import javax.ws.rs.PUT;
import javax.ws.rs.Path;
import javax.ws.rs.PathParam;
import javax.ws.rs.Produces;
import javax.ws.rs.core.Context;
import javax.ws.rs.core.UriInfo;

/**
 *
 * @author vinicius
 */
@Stateless
@Path("agendamento")
public class AgendamentoResource {
    @PersistenceContext(unitName = "StyleHairPU")
    private EntityManager em;
    
    
    private Gson gson = new Gson();
    
    @Context
    private UriInfo context;
    
    
    @GET
    @Produces("application/json")
    public String getJson() {
        AgendamentoDAO dao = new AgendamentoDAO(em);
        List<Agendamento> agendamentos;
        agendamentos = dao.buscarTodosAgendamentos();
       
      
        return gson.toJson(agendamentos);
    }
    
    @GET
    @Path("{agendamentoId}")
    @Produces("application/json")
    public String getAgendamento(@PathParam("agendamentoId") String id){
        System.out.println("pegando o cliente");
        Long n = Long.parseLong(id);
        System.out.println(n);
        AgendamentoDAO dao = new AgendamentoDAO(em);
        Agendamento agend = dao.consultarPorId(Agendamento.class, Long.parseLong(id));
   
        return gson.toJson(agend);
       
    }
    
    @GET
    @Path("{buscardata}/{dia}/{mes}/{ano}")
    @Produces("application/json")
    public String getAgendamentoPorData(@PathParam("dia") String dia,@PathParam("mes") String mes,@PathParam("ano") String ano ) {
        AgendamentoDAO dao = new AgendamentoDAO(em);
         List<Agendamento> agendamentos;
         SimpleDateFormat dateFormat_hora = new SimpleDateFormat("HH:mm");
         Date data = new Date();
         String horaAtual = dateFormat_hora.format(data);
         System.out.println("hora Atual" + horaAtual);
         Date d1 = new Date();
         SimpleDateFormat dateFormataData = new SimpleDateFormat("dd/MM/yyyy");
         String dataHoje = dateFormataData.format(d1);
         System.out.println("dataHoje ----" + dataHoje + "-------- " + dia+"/"+mes+"/"+ano );
         
         if(dataHoje.equalsIgnoreCase(dia+"/"+mes+"/"+ano)){

            agendamentos = dao.buscarAgendamentoPorData(dia+"/"+mes+"/"+ano + " ",horaAtual);
            return gson.toJson(agendamentos);
         }
         
         agendamentos = dao.buscarAgendamentoPorData(dia+"/"+mes+"/"+ano + " ","08:00");
      
        return gson.toJson(agendamentos);
    }
    
    
    @POST   
    @Consumes("application/json")
    @Produces("application/json")
    public String salvarAgendamento(String agendamento) throws Exception{
        
            Agendamento ag1 = gson.fromJson(agendamento, Agendamento.class);
            AgendamentoDAO dao = new AgendamentoDAO(em);
            return gson.toJson(dao.salvar(ag1));
        
    }
    
    @PUT
    @Consumes("application/json")
    public void atualizarAgendamento(String agendamento) throws Exception {
        salvarAgendamento(agendamento);
    }

}
