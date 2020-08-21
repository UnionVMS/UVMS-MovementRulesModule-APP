package eu.europa.ec.fisheries.uvms.movementrules.rest.service.arquillian;

import eu.europa.ec.fisheries.schema.movementrules.customrule.v1.AvailabilityType;
import eu.europa.ec.fisheries.schema.movementrules.customrule.v1.CustomRuleType;
import eu.europa.ec.fisheries.schema.movementrules.module.v1.GetCustomRuleListByQueryResponse;
import eu.europa.ec.fisheries.schema.movementrules.search.v1.CustomRuleListCriteria;
import eu.europa.ec.fisheries.schema.movementrules.search.v1.CustomRuleQuery;
import eu.europa.ec.fisheries.schema.movementrules.search.v1.CustomRuleSearchKey;
import eu.europa.ec.fisheries.schema.movementrules.search.v1.ListPagination;
import eu.europa.ec.fisheries.uvms.movementrules.rest.service.BuildRulesRestDeployment;
import eu.europa.ec.fisheries.uvms.movementrules.rest.service.RulesTestHelper;
import org.jboss.arquillian.container.test.api.OperateOnDeployment;
import org.jboss.arquillian.junit.Arquillian;
import org.junit.Ignore;
import org.junit.Test;
import org.junit.runner.RunWith;

import javax.ws.rs.client.Entity;
import javax.ws.rs.core.GenericType;
import javax.ws.rs.core.HttpHeaders;
import javax.ws.rs.core.MediaType;

import java.util.UUID;

import static org.junit.Assert.*;

@RunWith(Arquillian.class)
public class CustomRulesRestResourceQueryTest extends BuildRulesRestDeployment {

    @Test
    @OperateOnDeployment("normal")
    public void getCustomRulesByQueryTest() {
        CustomRuleType customRule = RulesTestHelper.getCompleteNewCustomRule();
        CustomRuleType created = createCustomRule(customRule);

        CustomRuleQuery customRuleQuery = new CustomRuleQuery();
        setBasicPagination(customRuleQuery);

        customRuleQuery.getCustomRuleSearchCriteria()
                .add(createCustomRuleCriteria(CustomRuleSearchKey.NAME, created.getName()));
        customRuleQuery.setDynamic(true);

        GetCustomRuleListByQueryResponse response = sendCustomRuleQuery(customRuleQuery);

        assertTrue(response.getCustomRules().stream()
                .anyMatch(cr -> created.getGuid().equals(cr.getGuid())));

        assertTrue(response.getCustomRules().stream()
                .anyMatch(cr -> created.getName().equals(cr.getName())));

    }

    @Test
    @OperateOnDeployment("normal")
    @Ignore("Type is probably referencing SubscriptionTypeType that is in RuleSubscription but the query points to customRule itself")
    public void getCustomRulesByTypeTest() {
        CustomRuleType customRule = RulesTestHelper.getCompleteNewCustomRule();
        CustomRuleType created = createCustomRule(customRule);

        CustomRuleQuery customRuleQuery = new CustomRuleQuery();
        setBasicPagination(customRuleQuery);

        customRuleQuery.getCustomRuleSearchCriteria()
                .add(createCustomRuleCriteria(CustomRuleSearchKey.TYPE, created.getSubscriptions().get(0).getType().toString()));
        customRuleQuery.setDynamic(true);

        GetCustomRuleListByQueryResponse response = sendCustomRuleQuery(customRuleQuery);

        assertTrue(response.getCustomRules().stream()
                .anyMatch(cr -> created.getGuid().equals(cr.getGuid())));
    }

    @Test
    @OperateOnDeployment("normal")
    public void getCustomRulesByNameAndGuid() {
        CustomRuleType customRule = RulesTestHelper.getCompleteNewCustomRule();
        CustomRuleType created = createCustomRule(customRule);

        CustomRuleQuery customRuleQuery = new CustomRuleQuery();
        setBasicPagination(customRuleQuery);

        customRuleQuery.getCustomRuleSearchCriteria()
                .add(createCustomRuleCriteria(CustomRuleSearchKey.NAME, created.getName()));
        customRuleQuery.getCustomRuleSearchCriteria()
                .add(createCustomRuleCriteria(CustomRuleSearchKey.GUID, created.getGuid()));
        customRuleQuery.setDynamic(true);

        GetCustomRuleListByQueryResponse response = sendCustomRuleQuery(customRuleQuery);

        assertTrue(response.getCustomRules().stream()
                .anyMatch(cr -> created.getGuid().equals(cr.getGuid())));
    }

    @Test
    @OperateOnDeployment("normal")
    public void getCustomRulesByTwoDifferentNames() {
        CustomRuleType customRule = RulesTestHelper.getCompleteNewCustomRule();
        CustomRuleType created1 = createCustomRule(customRule);

        customRule = RulesTestHelper.getCompleteNewCustomRule();
        CustomRuleType created2 = createCustomRule(customRule);

        CustomRuleQuery customRuleQuery = new CustomRuleQuery();
        setBasicPagination(customRuleQuery);

        customRuleQuery.getCustomRuleSearchCriteria()
                .add(createCustomRuleCriteria(CustomRuleSearchKey.NAME, created1.getName()));
        customRuleQuery.getCustomRuleSearchCriteria()
                .add(createCustomRuleCriteria(CustomRuleSearchKey.NAME, created2.getName()));
        customRuleQuery.setDynamic(true);

        GetCustomRuleListByQueryResponse response = sendCustomRuleQuery(customRuleQuery);

        assertTrue(response.getCustomRules().stream()
                .anyMatch(cr -> created1.getGuid().equals(cr.getGuid())));

        assertTrue(response.getCustomRules().stream()
                .anyMatch(cr -> created2.getGuid().equals(cr.getGuid())));
    }

    @Test
    @OperateOnDeployment("normal")
    public void getCustomRulesByTicketActionUserOrRuleUser() {
        //TicketActionUser is grossly missnamed, according to the implementation it is referencing 'value' in ruleAction
        CustomRuleType customRule = RulesTestHelper.getCompleteNewCustomRule();
        String ticketActionUser = "ticket action user: " + UUID.randomUUID().getLeastSignificantBits();
        customRule.getActions().get(0).setValue(ticketActionUser);
        CustomRuleType created1 = createCustomRule(customRule);

        customRule = RulesTestHelper.getCompleteNewCustomRule();
        customRule.setUpdatedBy("Another test user");
        CustomRuleType created2 = createCustomRule(customRule);

        CustomRuleQuery customRuleQuery = new CustomRuleQuery();
        setBasicPagination(customRuleQuery);

        customRuleQuery.getCustomRuleSearchCriteria()
                .add(createCustomRuleCriteria(CustomRuleSearchKey.TICKET_ACTION_USER, ticketActionUser));
        customRuleQuery.getCustomRuleSearchCriteria()
                .add(createCustomRuleCriteria(CustomRuleSearchKey.RULE_USER, created2.getUpdatedBy()));
        customRuleQuery.setDynamic(false);

        GetCustomRuleListByQueryResponse response = sendCustomRuleQuery(customRuleQuery);

        assertTrue(response.getCustomRules().stream()
                .anyMatch(cr -> created1.getGuid().equals(cr.getGuid())));

        assertTrue(response.getCustomRules().stream()
                .anyMatch(cr -> created2.getGuid().equals(cr.getGuid())));
    }

    @Test
    @OperateOnDeployment("normal")
    public void getCustomRulesByAvailabilityAndNameGetOnlyOneRule() {
        CustomRuleType customRule = RulesTestHelper.getCompleteNewCustomRule();
        customRule.setAvailability(AvailabilityType.GLOBAL);
        CustomRuleType created1 = createCustomRule(customRule);

        customRule = RulesTestHelper.getCompleteNewCustomRule();
        customRule.setAvailability(AvailabilityType.GLOBAL);
        CustomRuleType created2 = createCustomRule(customRule);

        CustomRuleQuery customRuleQuery = new CustomRuleQuery();
        setBasicPagination(customRuleQuery);

        customRuleQuery.getCustomRuleSearchCriteria()
                .add(createCustomRuleCriteria(CustomRuleSearchKey.AVAILABILITY, AvailabilityType.GLOBAL.value()));
        customRuleQuery.getCustomRuleSearchCriteria()
                .add(createCustomRuleCriteria(CustomRuleSearchKey.NAME, created1.getName()));
        customRuleQuery.setDynamic(true);

        GetCustomRuleListByQueryResponse response = sendCustomRuleQuery(customRuleQuery);

        assertTrue(response.getCustomRules().stream()
                .anyMatch(cr -> created1.getGuid().equals(cr.getGuid())));

        assertFalse(response.getCustomRules().stream()
                .anyMatch(cr -> created2.getGuid().equals(cr.getGuid())));
    }

    @Test
    @OperateOnDeployment("normal")
    public void getCustomRulesByAvailabilityOrName() {
        CustomRuleType customRule = RulesTestHelper.getCompleteNewCustomRule();
        customRule.setAvailability(AvailabilityType.GLOBAL);
        CustomRuleType created1 = createCustomRule(customRule);

        customRule = RulesTestHelper.getCompleteNewCustomRule();
        customRule.setAvailability(AvailabilityType.PUBLIC);
        CustomRuleType created2 = createCustomRule(customRule);

        CustomRuleQuery customRuleQuery = new CustomRuleQuery();
        setBasicPagination(customRuleQuery);

        customRuleQuery.getCustomRuleSearchCriteria()
                .add(createCustomRuleCriteria(CustomRuleSearchKey.AVAILABILITY, AvailabilityType.GLOBAL.value()));
        customRuleQuery.getCustomRuleSearchCriteria()
                .add(createCustomRuleCriteria(CustomRuleSearchKey.NAME, created2.getName()));
        customRuleQuery.setDynamic(false);

        GetCustomRuleListByQueryResponse response = sendCustomRuleQuery(customRuleQuery);

        assertTrue(response.getCustomRules().stream()
                .anyMatch(cr -> created1.getGuid().equals(cr.getGuid())));

        assertTrue(response.getCustomRules().stream()
                .anyMatch(cr -> created2.getGuid().equals(cr.getGuid())));
    }

    private CustomRuleType createCustomRule(CustomRuleType customRule) {
        CustomRuleType created = getWebTarget()
                .path("/customrules")
                .request(MediaType.APPLICATION_JSON)
                .header(HttpHeaders.AUTHORIZATION, getToken())
                .post(Entity.json(customRule), new GenericType<CustomRuleType>() {
                });
        assertNotNull(created.getGuid());
        return created;
    }

    private GetCustomRuleListByQueryResponse sendCustomRuleQuery(CustomRuleQuery customRuleQuery) {
        GetCustomRuleListByQueryResponse response = getWebTarget()
                .path("/customrules/listByQuery")
                .request(MediaType.APPLICATION_JSON)
                .header(HttpHeaders.AUTHORIZATION, getToken())
                .post(Entity.json(customRuleQuery), GetCustomRuleListByQueryResponse.class);
        assertNotNull(response);
        return response;
    }

    private void setBasicPagination(CustomRuleQuery query){
        ListPagination pagination = new ListPagination();
        pagination.setListSize(10);
        pagination.setPage(1);
        query.setPagination(pagination);
    }

    private CustomRuleListCriteria createCustomRuleCriteria(CustomRuleSearchKey key, String value){
        CustomRuleListCriteria criteria = new CustomRuleListCriteria();
        criteria.setKey(key);
        criteria.setValue(value);

        return criteria;
    }
}
