package eu.europa.ec.fisheries.uvms.rules.rest.service;

import static org.hamcrest.CoreMatchers.is;
import static org.junit.Assert.assertThat;
import java.util.Arrays;
import javax.ws.rs.client.Entity;
import javax.ws.rs.core.MediaType;
import org.jboss.arquillian.container.test.api.RunAsClient;
import org.jboss.arquillian.junit.Arquillian;
import org.junit.Test;
import org.junit.runner.RunWith;
import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.databind.node.ObjectNode;
import eu.europa.ec.fisheries.schema.rules.search.v1.ListPagination;
import eu.europa.ec.fisheries.schema.rules.search.v1.TicketListCriteria;
import eu.europa.ec.fisheries.schema.rules.search.v1.TicketQuery;
import eu.europa.ec.fisheries.schema.rules.search.v1.TicketSearchKey;
import eu.europa.ec.fisheries.schema.rules.source.v1.GetTicketListByMovementsResponse;
import eu.europa.ec.fisheries.schema.rules.source.v1.GetTicketListByQueryResponse;
import eu.europa.ec.fisheries.uvms.rules.rest.service.Arquillian.BuildAssetServiceDeployment;

@RunWith(Arquillian.class)
public class TicketRestResourceTest extends BuildAssetServiceDeployment {

    @Test
    @RunAsClient
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
    @RunAsClient
    public void getTicketsByMovementsTest() throws Exception {
        String response = getWebTarget()
                .path("tickets/listByMovements")
                .request(MediaType.APPLICATION_JSON)
                .post(Entity.json(Arrays.asList("TEST_GUID")), String.class);
        
        GetTicketListByMovementsResponse ticketList = deserializeResponseDto(response, GetTicketListByMovementsResponse.class);
        assertThat(ticketList.getTickets().size(), is(0));
    }

    @Test
    @RunAsClient
    public void countTicketsByMovementsTest() throws Exception {
        String response = getWebTarget()
                .path("tickets/countByMovements")
                .request(MediaType.APPLICATION_JSON)
                .post(Entity.json(Arrays.asList("TEST_GUID")), String.class);
        
        Long ticketList = deserializeResponseDto(response, Long.class);
        assertThat(ticketList, is(0L));
    }
    
    
//    @PUT
//    @Consumes(value = { MediaType.APPLICATION_JSON })
//    @Produces(value = { MediaType.APPLICATION_JSON })
//    @Path("/status")
//    @RequiresFeature(UnionVMSFeature.manageAlarmsOpenTickets)
//    public ResponseDto updateTicketStatus(final TicketType ticketType) {
    
    
//    @POST
//    @Consumes(value = { MediaType.APPLICATION_JSON })
//    @Produces(value = { MediaType.APPLICATION_JSON })
//    @Path("/status/{loggedInUser}/{status}")
//    @RequiresFeature(UnionVMSFeature.manageAlarmsOpenTickets)
//    public ResponseDto updateTicketStatusByQuery(@PathParam("loggedInUser") String loggedInUser, TicketQuery query, @PathParam("status") TicketStatusType status) {
    
    
//    @GET
//    @Produces(value = { MediaType.APPLICATION_JSON })
//    @Path("/{guid}")
//    @RequiresFeature(UnionVMSFeature.viewAlarmsOpenTickets)
//    public ResponseDto getTicketByGuid(@PathParam("guid") String guid) {
    
    
//    @GET
//    @Produces(value = { MediaType.APPLICATION_JSON })
//    @Path("/countopen/{loggedInUser}")
//    @RequiresFeature(UnionVMSFeature.viewAlarmsOpenTickets)
//    public ResponseDto getNumberOfOpenTicketReports(@PathParam(value = "loggedInUser") final String loggedInUser) {
    
    
//    @GET
//    @Produces(value = { MediaType.APPLICATION_JSON })
//    @Path("/countAssetsNotSending")
//    @RequiresFeature(UnionVMSFeature.viewAlarmsOpenTickets)
//    public ResponseDto getNumberOfAssetsNotSending() {
    
    
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
