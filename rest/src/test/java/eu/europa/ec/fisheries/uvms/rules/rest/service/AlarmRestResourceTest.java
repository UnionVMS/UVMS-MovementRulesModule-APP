package eu.europa.ec.fisheries.uvms.rules.rest.service;

import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.CoreMatchers.notNullValue;
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
import eu.europa.ec.fisheries.schema.rules.module.v1.GetAlarmListByQueryResponse;
import eu.europa.ec.fisheries.schema.rules.search.v1.AlarmListCriteria;
import eu.europa.ec.fisheries.schema.rules.search.v1.AlarmQuery;
import eu.europa.ec.fisheries.schema.rules.search.v1.AlarmSearchKey;
import eu.europa.ec.fisheries.schema.rules.search.v1.ListPagination;
import eu.europa.ec.fisheries.uvms.rules.rest.service.Arquillian.BuildRulesRestDeployment;

//@RunAsClient
@RunWith(Arquillian.class)
public class AlarmRestResourceTest extends BuildRulesRestDeployment {

    @Test
    public void getAlarmListTest() throws Exception {
        AlarmQuery basicAlarmQuery = getBasicAlarmQuery();
        AlarmListCriteria criteria = new AlarmListCriteria();
        criteria.setKey(AlarmSearchKey.RULE_GUID);
        criteria.setValue("TEST_GUID");
        basicAlarmQuery.getAlarmSearchCriteria().add(criteria);
        
        String response = getWebTarget()
                .path("alarms/list")
                .request(MediaType.APPLICATION_JSON)
                .post(Entity.json(basicAlarmQuery), String.class);
        
        GetAlarmListByQueryResponse alarmList = deserializeResponseDto(response, GetAlarmListByQueryResponse.class);
        assertThat(alarmList.getAlarms().size(), is(0));
    }

/*
    @Test
    @RunAsClient
    public void updateAlarmStatusTest() throws Exception {
        String response = getWebTarget()
                .path("alarms")
                .request(MediaType.APPLICATION_JSON)
                .put(Entity.json(new AlarmReportType()), String.class);
        
        assertThat(response, is(notNullValue()));
    }
*/
    
    /*
    @Test
    @RunAsClient
    public void getAlarmReportByGuidTest() throws Exception {
        String response = getWebTarget()
                .path("alarms/" + "NULL_GUID")
                .request(MediaType.APPLICATION_JSON)
                .get(String.class);
        
        String responseString = deserializeResponseDto(response, String.class);
        assertThat(responseString, is(notNullValue()));
    }
    */
    
    
    @Test
    public void reprocessAlarmTest() throws Exception {
        String response = getWebTarget()
                .path("alarms/reprocess")
                .request(MediaType.APPLICATION_JSON)
                .post(Entity.json(Arrays.asList("NULL_GUID")), String.class);
        
        String responseString = deserializeResponseDto(response, String.class);
        assertThat(responseString, is("OK"));
    }
    
    
    @Test
    public void getNumberOfOpenAlarmReportsTest() throws Exception {
        String response = getWebTarget()
                .path("alarms/countopen")
                .request(MediaType.APPLICATION_JSON)
                .get(String.class);
        
        Integer openAlarmReports = deserializeResponseDto(response, Integer.class);
        assertThat(openAlarmReports, is(notNullValue()));
    }
    
    private static AlarmQuery getBasicAlarmQuery() {
        AlarmQuery query = new AlarmQuery();
        query.setDynamic(true);
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
