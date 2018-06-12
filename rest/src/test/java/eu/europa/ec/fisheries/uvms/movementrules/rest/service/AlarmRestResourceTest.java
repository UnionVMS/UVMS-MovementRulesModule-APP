package eu.europa.ec.fisheries.uvms.movementrules.rest.service;

import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.CoreMatchers.notNullValue;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertThat;
import java.util.Arrays;
import java.util.Date;
import java.util.UUID;
import javax.inject.Inject;
import javax.ws.rs.client.Entity;
import javax.ws.rs.core.MediaType;

import eu.europa.ec.fisheries.schema.movementrules.alarm.v1.AlarmReportType;
import eu.europa.ec.fisheries.schema.movementrules.alarm.v1.AlarmStatusType;
import eu.europa.ec.fisheries.uvms.movementrules.service.dao.RulesDao;
import eu.europa.ec.fisheries.uvms.movementrules.service.entity.AlarmReport;
import eu.europa.ec.fisheries.uvms.movementrules.service.entity.RawMovement;
import eu.europa.ec.fisheries.uvms.movementrules.service.mapper.AlarmMapper;
import org.jboss.arquillian.container.test.api.RunAsClient;
import org.jboss.arquillian.junit.Arquillian;
import org.junit.Test;
import org.junit.runner.RunWith;
import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.databind.node.ObjectNode;
import eu.europa.ec.fisheries.schema.movementrules.module.v1.GetAlarmListByQueryResponse;
import eu.europa.ec.fisheries.schema.movementrules.search.v1.AlarmListCriteria;
import eu.europa.ec.fisheries.schema.movementrules.search.v1.AlarmQuery;
import eu.europa.ec.fisheries.schema.movementrules.search.v1.AlarmSearchKey;
import eu.europa.ec.fisheries.schema.movementrules.search.v1.ListPagination;
import eu.europa.ec.fisheries.uvms.movementrules.rest.service.arquillian.BuildRulesRestDeployment;


//@RunAsClient
@RunWith(Arquillian.class)
public class AlarmRestResourceTest extends BuildRulesRestDeployment {


    @Inject
    RulesDao rulesDao;

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
        assertThat(alarmList.getAlarms().size(), is(notNullValue()));

        int prevNumberOfReports = alarmList.getAlarms().size();

        AlarmReport alarmReport = getBasicAlarmReport();
        AlarmReport createdAlarmReport = rulesDao.createAlarmReport(alarmReport);
        criteria.setKey(AlarmSearchKey.ALARM_GUID);
        criteria.setValue(createdAlarmReport.getGuid());

        response = getWebTarget()
                .path("alarms/list")
                .request(MediaType.APPLICATION_JSON)
                .post(Entity.json(basicAlarmQuery), String.class);

        alarmList = deserializeResponseDto(response, GetAlarmListByQueryResponse.class);
        assertThat(alarmList.getAlarms().size(), is(prevNumberOfReports + 1));
        assertEquals(createdAlarmReport.getGuid(), alarmList.getAlarms().get(0).getGuid());
        assertEquals(createdAlarmReport.getStatus(), alarmList.getAlarms().get(0).getStatus().value());

        rulesDao.removeAlarmReportAfterTests(createdAlarmReport);
    }

    @Test
    public void negativeGetAlarmListTest() throws Exception{
        String response = getWebTarget()
                .path("alarms/list")
                .request(MediaType.APPLICATION_JSON)
                .post(Entity.json(new AlarmQuery()), String.class);

        assertEquals(500, getReturnCode(response));
    }


    @Test
    public void updateAlarmStatusTest() throws Exception {
        AlarmReport alarmReport = getBasicAlarmReport();
        AlarmReport createdAlarmReport = rulesDao.createAlarmReport(alarmReport);


        AlarmReportType input = AlarmMapper.toAlarmReportType(createdAlarmReport);
        input.setStatus(AlarmStatusType.REJECTED);

        String response = getWebTarget()
                .path("alarms")
                .request(MediaType.APPLICATION_JSON)
                .put(Entity.json(input), String.class);

        assertThat(response, is(notNullValue()));
        AlarmReportType output = deserializeResponseDto(response, AlarmReportType.class);
        assertEquals(input.getGuid(), output.getGuid());
        assertEquals(AlarmStatusType.REJECTED, output.getStatus());

        rulesDao.removeAlarmReportAfterTests(createdAlarmReport);

    }

    @Test
    public void negativeUpdateAlarmStatusTest() throws Exception {
        String response = getWebTarget()
                .path("alarms")
                .request(MediaType.APPLICATION_JSON)
                .put(Entity.json(new AlarmReportType()), String.class);

        assertEquals(500, getReturnCode(response));
    }





    @Test
    public void getAlarmReportByGuidTest() throws Exception {
        AlarmReport alarmReport = getBasicAlarmReport();
        AlarmReport createdAlarmReport = rulesDao.createAlarmReport(alarmReport);

        String response = getWebTarget()
                .path("alarms/" + createdAlarmReport.getGuid())
                .request(MediaType.APPLICATION_JSON)
                .get(String.class);
        
        AlarmReportType responseAlarmReportType = deserializeResponseDto(response, AlarmReportType.class);
        assertNotNull(responseAlarmReportType);
        assertEquals(createdAlarmReport.getGuid(), responseAlarmReportType.getGuid());

        rulesDao.removeAlarmReportAfterTests(createdAlarmReport);
    }

    @Test
    public void negativeGetAlarmReportByGuidTest() throws Exception{
        String response = getWebTarget()
                .path("alarms/" + "test guid")
                .request(MediaType.APPLICATION_JSON)
                .get(String.class);

        assertEquals(500, getReturnCode(response));
    }
    
    
    @Test
    public void reprocessAlarmTest() throws Exception {
        String response = getWebTarget()
                .path("alarms/reprocess")
                .request(MediaType.APPLICATION_JSON)
                .post(Entity.json(Arrays.asList("NULL_GUID")), String.class);
        
        String responseString = deserializeResponseDto(response, String.class);
        assertThat(responseString, is("OK"));

        AlarmReport alarmReport = getBasicAlarmReport();
        RawMovement rawMovement = new RawMovement();
        rawMovement.setUpdated(new Date());
        rawMovement.setUpdatedBy("Test User");
        rawMovement.setActive(true);
        rawMovement.setAlarmReport(alarmReport);
        alarmReport.setRawMovement(rawMovement);
        AlarmReport createdAlarmReport = rulesDao.createAlarmReport(alarmReport);

        response = getWebTarget()
                .path("alarms/reprocess")
                .request(MediaType.APPLICATION_JSON)
                .post(Entity.json(Arrays.asList(createdAlarmReport.getGuid())), String.class);

        responseString = deserializeResponseDto(response, String.class);
        assertThat(responseString, is("OK"));

        response = getWebTarget()
                .path("alarms/" + createdAlarmReport.getGuid())
                .request(MediaType.APPLICATION_JSON)
                .get(String.class);

        AlarmReportType responseAlarmReportType = deserializeResponseDto(response, AlarmReportType.class);
        assertNotNull(responseAlarmReportType);
        assertEquals(AlarmStatusType.REPROCESSED, responseAlarmReportType.getStatus());


        rulesDao.removeAlarmReportAfterTests(createdAlarmReport);
    }
    
    
    @Test
    public void getNumberOfOpenAlarmReportsTest() throws Exception {
        String response = getWebTarget()
                .path("alarms/countopen")
                .request(MediaType.APPLICATION_JSON)
                .get(String.class);
        
        Integer openAlarmReports = deserializeResponseDto(response, Integer.class);
        assertThat(openAlarmReports, is(notNullValue()));

        int prevNumberOfReports = openAlarmReports;

        AlarmReport alarmReport = getBasicAlarmReport();
        AlarmReport createdAlarmReport = rulesDao.createAlarmReport(alarmReport);

        //hmm, is it a good idea to have tests that depend on there not being crap in teh DB?
        response = getWebTarget()
                .path("alarms/countopen")
                .request(MediaType.APPLICATION_JSON)
                .get(String.class);

        openAlarmReports = deserializeResponseDto(response, Integer.class);
        assertThat(openAlarmReports, is(prevNumberOfReports + 1 ));

        AlarmReport alarmReport2 = getBasicAlarmReport();
        AlarmReport createdAlarmReport2 = rulesDao.createAlarmReport(alarmReport2);

        response = getWebTarget()
                .path("alarms/countopen")
                .request(MediaType.APPLICATION_JSON)
                .get(String.class);

        openAlarmReports = deserializeResponseDto(response, Integer.class);
        assertThat(openAlarmReports, is(prevNumberOfReports + 2 ));

        rulesDao.removeAlarmReportAfterTests(createdAlarmReport);
        rulesDao.removeAlarmReportAfterTests(createdAlarmReport2);
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

    private AlarmReport getBasicAlarmReport() {
        AlarmReport alarmReport = new AlarmReport();
        alarmReport.setAssetGuid(UUID.randomUUID().toString());
        alarmReport.setStatus(AlarmStatusType.OPEN.value());
        alarmReport.setUpdated(new Date());
        alarmReport.setUpdatedBy("Test user");
        return alarmReport;
    }

    ObjectMapper objectMapper = new ObjectMapper();
    private int getReturnCode(String responsDto) throws Exception{
        return objectMapper.readValue(responsDto, ObjectNode.class).get("code").asInt();
    }

    private static <T> T deserializeResponseDto(String responseDto, Class<T> clazz) throws Exception {
        ObjectMapper objectMapper = new ObjectMapper();
        ObjectNode node = objectMapper.readValue(responseDto, ObjectNode.class);
        JsonNode jsonNode = node.get("data");
        return objectMapper.readValue(objectMapper.writeValueAsString(jsonNode), clazz);
    }
}
