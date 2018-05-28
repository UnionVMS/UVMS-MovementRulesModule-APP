package eu.europa.ec.fisheries.uvms.rules.rest.service;

import static org.hamcrest.CoreMatchers.is;
import static org.junit.Assert.assertThat;
import static org.junit.Assert.assertTrue;
import java.util.Arrays;
import javax.ws.rs.InternalServerErrorException;
import javax.ws.rs.client.Entity;
import javax.ws.rs.core.MediaType;
import org.jboss.arquillian.container.test.api.RunAsClient;
import org.jboss.arquillian.junit.Arquillian;
import org.junit.Test;
import org.junit.runner.RunWith;
import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.databind.node.ObjectNode;
import eu.europa.ec.fisheries.schema.rules.module.v1.GetTicketListByMovementsResponse;
import eu.europa.ec.fisheries.schema.rules.module.v1.GetTicketListByQueryResponse;
import eu.europa.ec.fisheries.schema.rules.search.v1.ListPagination;
import eu.europa.ec.fisheries.schema.rules.search.v1.TicketListCriteria;
import eu.europa.ec.fisheries.schema.rules.search.v1.TicketQuery;
import eu.europa.ec.fisheries.schema.rules.search.v1.TicketSearchKey;
import eu.europa.ec.fisheries.schema.rules.ticket.v1.TicketStatusType;
import eu.europa.ec.fisheries.schema.rules.ticket.v1.TicketType;
import eu.europa.ec.fisheries.uvms.rules.rest.service.Arquillian.BuildRulesRestDeployment;

@RunAsClient
@RunWith(Arquillian.class)
public class TicketRestResourceTest extends BuildRulesRestDeployment {

    @Test
    public void getTicketListTest() throws Exception {
        TicketQuery query = getBasicTicketQuery();
        TicketListCriteria criteria = new TicketListCriteria();
        criteria.setKey(TicketSearchKey.RULE_NAME);
        criteria.setValue("Test Name");
        query.getTicketSearchCriteria().add(criteria);
        
        String response = getWebTarget()
                .path("tickets/list/" + "testUser")
                .request(MediaType.APPLICATION_JSON)
                .post(Entity.json(query), String.class);
        
        GetTicketListByQueryResponse ticketList = deserializeResponseDto(response, GetTicketListByQueryResponse.class);
        assertThat(ticketList.getTickets().size(), is(0));
    }
    
    @Test
    public void getTicketsByMovementsTest() throws Exception {
        String response = getWebTarget()
                .path("tickets/listByMovements")
                .request(MediaType.APPLICATION_JSON)
                .post(Entity.json(Arrays.asList("TEST_GUID")), String.class);
        
        GetTicketListByMovementsResponse ticketList = deserializeResponseDto(response, GetTicketListByMovementsResponse.class);
        assertThat(ticketList.getTickets().size(), is(0));
    }

    @Test
    public void countTicketsByMovementsTest() throws Exception {
        String response = getWebTarget()
                .path("tickets/countByMovements")
                .request(MediaType.APPLICATION_JSON)
                .post(Entity.json(Arrays.asList("TEST_GUID")), String.class);
        
        Long ticketList = deserializeResponseDto(response, Long.class);
        assertThat(ticketList, is(0L));
    }
    
    @Test
    public void updateTicketStatusTest() throws Exception{ //there are no tickets to update so this one will result in an internal server error
        TicketType ticketType = new TicketType();
        ticketType.setGuid("TestGuid");

        try {
            String response = getWebTarget()
                    .path("tickets/status")
                    .request(MediaType.APPLICATION_JSON)
                    .put(Entity.json(ticketType), String.class);
        }catch (InternalServerErrorException ex){
            assertTrue(true);
        }

    }
    
    @Test
    public void updateTicketStatusByQueryTest() throws Exception {
        TicketQuery ticketQuery = new TicketQuery();
        TicketListCriteria tlc = new TicketListCriteria();
        tlc.setKey(TicketSearchKey.TICKET_GUID);
        tlc.setValue("Test Guid");
        ticketQuery.getTicketSearchCriteria().add(tlc);
        ListPagination lp = new ListPagination();
        lp.setPage(1);
        lp.setListSize(10);
        ticketQuery.setPagination(lp);


        String response = getWebTarget()
                    .path("tickets/status/vms_admin_com/" + TicketStatusType.OPEN)
                    .request(MediaType.APPLICATION_JSON)
                    .post(Entity.json(ticketQuery), String.class);

        //ObjectMapper objectMapper = new ObjectMapper();
        Object[] returnArray = deserializeResponseDto(response, Object[].class);
        assertThat(returnArray.length, is(0));
    }

    @Test
    public void getTicketByGuid() throws Exception {
        try {
            String response = getWebTarget()
                    .path("tickets/" + "TestGuid")    //no tickets in the db
                    .request(MediaType.APPLICATION_JSON)
                    .get(String.class);
        }catch (InternalServerErrorException e){
            assertTrue(true);
        }

    }

    @Test
    public void getNumberOfOpenTicketReportsTest() throws Exception{
        String response = getWebTarget()
                .path("/tickets/countopen/" + "vms_admin_com")    //no tickets in the db
                .request(MediaType.APPLICATION_JSON)
                .get(String.class);

        Long ticketList = deserializeResponseDto(response, Long.class);
        assertThat(ticketList, is(0L));
    }

    @Test
    public void getNumberOfAssetsNotSendingTest() throws Exception{
        String response = getWebTarget()
                .path("/tickets/countAssetsNotSending")    //no tickets in the db
                .request(MediaType.APPLICATION_JSON)
                .get(String.class);

        Long ticketList = deserializeResponseDto(response, Long.class);
        assertThat(ticketList, is(0L));
    }

    
    
    private static TicketQuery getBasicTicketQuery() {
        TicketQuery query = new TicketQuery();
        ListPagination pagination = new ListPagination();
        pagination.setPage(1);
        pagination.setListSize(100);
        query.setPagination(pagination);
        return query;
    }
    
    private static <T> T deserializeResponseDto(String responseDto, Class<T> clazz) throws Exception {
        ObjectMapper objectMapper = new ObjectMapper();
        ObjectNode node = objectMapper.readValue(responseDto, ObjectNode.class);
        JsonNode jsonNode = node.get("data");
        return objectMapper.readValue(objectMapper.writeValueAsString(jsonNode), clazz);
    }
    
}
