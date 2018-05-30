package eu.europa.ec.fisheries.uvms.rules.rest.service.Arquillian;

import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.databind.node.ObjectNode;
import eu.europa.ec.fisheries.schema.rules.customrule.v1.*;
import eu.europa.ec.fisheries.schema.rules.search.v1.AlarmListCriteria;
import eu.europa.ec.fisheries.schema.rules.search.v1.AlarmQuery;
import eu.europa.ec.fisheries.schema.rules.search.v1.AlarmSearchKey;
import eu.europa.ec.fisheries.schema.rules.search.v1.ListPagination;
import eu.europa.ec.fisheries.uvms.rules.rest.dto.ResponseDto;
import eu.europa.ec.fisheries.uvms.rules.rest.service.AlarmRestResource;
import org.jboss.arquillian.junit.Arquillian;
import org.junit.Assert;
import org.junit.Test;
import org.junit.runner.RunWith;

import eu.europa.ec.fisheries.uvms.rules.rest.service.CustomRulesRestResource;


import javax.ejb.EJB;
import javax.ws.rs.client.Entity;
import javax.ws.rs.core.MediaType;

@RunWith(Arquillian.class)
public class CoverageTest extends TransactionalTests {

    @EJB
    CustomRulesRestResource tmp;

    @EJB
    AlarmRestResource alarm;

    @Test
    public void createAndDeleteCustomRuleTest() throws Exception{
     /*   CustomRuleType customRule = getCompleteNewCustomRule();


        //String response = getWebTarget().path("/customrules").request(MediaType.APPLICATION_JSON).post(Entity.json(customRule), String.class);
        ResponseDto response = tmp.createCustomRule(customRule);
        //Assert.assertEquals(200, response.getCode());
        String returnStr = (String) response.getData();
        System.out.println(returnStr);

        //CustomRuleType returnCrt = deserializeResponseDto(returnStr, CustomRuleType.class);
        //Assert.assertEquals(customRule.getName(), returnCrt.getName());

        //String deleteResponse = getWebTarget().path("/customrules/" + returnCrt.getGuid()).request(MediaType.APPLICATION_JSON).delete(String.class);
        //ResponseDto deleteResponse = tmp.deleteCustomRule(returnCrt.getGuid());

        //Assert.assertEquals(200, getReturnCode(deleteResponse));
*/


        AlarmQuery basicAlarmQuery = getBasicAlarmQuery();
        AlarmListCriteria criteria = new AlarmListCriteria();
        criteria.setKey(AlarmSearchKey.RULE_GUID);
        criteria.setValue("TEST_GUID");
        basicAlarmQuery.getAlarmSearchCriteria().add(criteria);

        alarm.getAlarmList(basicAlarmQuery);
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

    private CustomRuleType getCompleteNewCustomRule(){
        CustomRuleType customRule = new CustomRuleType();

        customRule.setName("Flag SWE && area DNK => Send to DNK" + " (" + System.currentTimeMillis() + ")");
        customRule.setAvailability(AvailabilityType.PRIVATE);
        customRule.setUpdatedBy("vms_admin_com");
        customRule.setActive(true);
        customRule.setArchived(false);

        // If flagstate = SWE
        CustomRuleSegmentType flagStateRule = new CustomRuleSegmentType();
        flagStateRule.setStartOperator("(");
        flagStateRule.setCriteria(CriteriaType.ASSET);
        flagStateRule.setSubCriteria(SubCriteriaType.FLAG_STATE);
        flagStateRule.setCondition(ConditionType.EQ);
        flagStateRule.setValue("SWE");
        flagStateRule.setEndOperator(")");
        flagStateRule.setLogicBoolOperator(LogicOperatorType.AND);
        flagStateRule.setOrder("0");
        customRule.getDefinitions().add(flagStateRule);

        // and area = DNK
        CustomRuleSegmentType areaRule = new CustomRuleSegmentType();
        areaRule.setStartOperator("(");
        areaRule.setCriteria(CriteriaType.AREA);
        areaRule.setSubCriteria(SubCriteriaType.AREA_CODE);
        areaRule.setCondition(ConditionType.EQ);
        areaRule.setValue("DNK");
        areaRule.setEndOperator(")");
        areaRule.setLogicBoolOperator(LogicOperatorType.NONE);
        areaRule.setOrder("1");
        customRule.getDefinitions().add(areaRule);

        // then send to FLUX DNK
        CustomRuleActionType action = new CustomRuleActionType();
        action.setAction(ActionType.SEND_TO_FLUX);
        action.setValue("FLUX DNK");
        action.setOrder("0");

        customRule.getActions().add(action);

        return customRule;
    }
}
