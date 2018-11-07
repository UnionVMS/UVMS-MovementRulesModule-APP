package eu.europa.ec.fisheries.uvms.movementrules.service.message.bean;

import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.CoreMatchers.notNullValue;
import static org.junit.Assert.assertThat;
import static org.junit.Assert.assertTrue;

import java.util.Calendar;
import java.util.Date;
import java.util.List;
import javax.inject.Inject;

import org.jboss.arquillian.container.test.api.OperateOnDeployment;
import org.jboss.arquillian.junit.Arquillian;
import org.junit.Test;
import org.junit.runner.RunWith;
import eu.europa.ec.fisheries.schema.movementrules.search.v1.CustomRuleListCriteria;
import eu.europa.ec.fisheries.schema.movementrules.search.v1.CustomRuleQuery;
import eu.europa.ec.fisheries.schema.movementrules.search.v1.CustomRuleSearchKey;
import eu.europa.ec.fisheries.uvms.commons.date.DateUtils;
import eu.europa.ec.fisheries.uvms.movementrules.service.RulesService;
import eu.europa.ec.fisheries.uvms.movementrules.service.RulesTestHelper;
import eu.europa.ec.fisheries.uvms.movementrules.service.TransactionalTests;
import eu.europa.ec.fisheries.uvms.movementrules.service.business.MovementFact;
import eu.europa.ec.fisheries.uvms.movementrules.service.business.RawMovementFact;
import eu.europa.ec.fisheries.uvms.movementrules.service.dto.CustomRuleListResponseDto;
import eu.europa.ec.fisheries.uvms.movementrules.service.entity.CustomRule;
import eu.europa.ec.fisheries.uvms.movementrules.service.entity.RuleAction;

@RunWith(Arquillian.class)
public class ValidationServiceBeanTest extends TransactionalTests {

    @Inject
    ValidationServiceBean validationService;

    @Inject
    RulesService rulesService;

    @Test
    @OperateOnDeployment("normal")
    public void getCustomRulesByUserTest() throws Exception {
        CustomRule customRule = RulesTestHelper.createCompleteCustomRule();
        CustomRule createdCustomRule = rulesService.createCustomRule(customRule, "", "");

        List<CustomRule> customRulesByUser = validationService.getCustomRulesByUser(createdCustomRule.getUpdatedBy());
        assertTrue(customRulesByUser.size() > 0);
    }

    @Test
    @OperateOnDeployment("normal")
    public void getRunnableCustomRulesTest() throws Exception {
        List<CustomRule> runnableCustomRulesBefore = validationService.getRunnableCustomRules();
        CustomRule customRule = RulesTestHelper.createCompleteCustomRule();
        rulesService.createCustomRule(customRule, "", "");

        List<CustomRule> runnableCustomRulesAfter = validationService.getRunnableCustomRules();
        assertThat(runnableCustomRulesAfter.size(), is(runnableCustomRulesBefore.size() + 1));
    }

    @Test
    @OperateOnDeployment("normal")
    public void getRunnableCustomRulesInactivateRuleTest() throws Exception {
        List<CustomRule> runnableCustomRulesBefore = validationService.getRunnableCustomRules();
        CustomRule customRule = RulesTestHelper.createCompleteCustomRule();
        CustomRule createdCustomRule = rulesService.createCustomRule(customRule, "", "");

        List<CustomRule> runnableCustomRulesAfter = validationService.getRunnableCustomRules();
        assertThat(runnableCustomRulesAfter.size(), is(runnableCustomRulesBefore.size() + 1));

        rulesService.deleteCustomRule(createdCustomRule.getGuid(), "Test", "", "");

        List<CustomRule> runnableCustomRulesAfterDelete = validationService.getRunnableCustomRules();
        assertThat(runnableCustomRulesAfterDelete.size(), is(runnableCustomRulesBefore.size()));
    }


    @Test
    @OperateOnDeployment("normal")
    public void getCustomRuleListByQueryGuidTest() throws Exception {
        CustomRule customRule = RulesTestHelper.createCompleteCustomRule();
        CustomRule createdCustomRule = rulesService.createCustomRule(customRule, "", "");

        CustomRuleQuery query = RulesTestHelper.createBasicCustomRuleQuery();
        CustomRuleListCriteria criteria = new CustomRuleListCriteria();
        criteria.setKey(CustomRuleSearchKey.GUID);
        criteria.setValue(createdCustomRule.getGuid());
        query.getCustomRuleSearchCriteria().add(criteria);

        CustomRuleListResponseDto customRulesResponse = validationService.getCustomRulesByQuery(query);
        List<CustomRule> customRules = customRulesResponse.getCustomRuleList();

        assertThat(customRules.size(), is(1));

        CustomRule fetchedCustomRule = customRules.get(0);
        assertThat(fetchedCustomRule.getGuid(), is(createdCustomRule.getGuid()));
        assertThat(fetchedCustomRule.getName(), is(createdCustomRule.getName()));
        assertThat(fetchedCustomRule.getUpdatedBy(), is(createdCustomRule.getUpdatedBy()));
    }

    @Test
    @OperateOnDeployment("normal")
    public void getCustomRuleListByQueryGuidTwoGuidsTest() throws Exception {
        CustomRule customRule = RulesTestHelper.createCompleteCustomRule();
        CustomRule createdCustomRule = rulesService.createCustomRule(customRule, "", "");

        CustomRule customRule2 = RulesTestHelper.createCompleteCustomRule();
        CustomRule createdCustomRule2 = rulesService.createCustomRule(customRule2, "", "");


        CustomRuleQuery query = RulesTestHelper.createBasicCustomRuleQuery();
        CustomRuleListCriteria criteria = new CustomRuleListCriteria();
        criteria.setKey(CustomRuleSearchKey.GUID);
        criteria.setValue(createdCustomRule.getGuid());
        query.getCustomRuleSearchCriteria().add(criteria);
        CustomRuleListCriteria criteria2 = new CustomRuleListCriteria();
        criteria2.setKey(CustomRuleSearchKey.GUID);
        criteria2.setValue(createdCustomRule2.getGuid());
        query.getCustomRuleSearchCriteria().add(criteria2);


        CustomRuleListResponseDto customRulesResponse = validationService.getCustomRulesByQuery(query);
        List<CustomRule> customRules = customRulesResponse.getCustomRuleList();

        assertThat(customRules.size(), is(2));
        assertTrue(customRules.stream()
                .anyMatch(r -> r.getGuid().equals(createdCustomRule.getGuid())));
        assertTrue(customRules.stream()
                .anyMatch(r -> r.getGuid().equals(createdCustomRule2.getGuid())));
    }

    @Test
    @OperateOnDeployment("normal")
    public void getCustomRuleListByQueryTicketActionUserTest() throws Exception {
        CustomRule customRule = RulesTestHelper.createCompleteCustomRule();
        String mail = "test@mail.com";
        RuleAction action = new RuleAction();
        action.setAction("EMAIL");
        action.setValue(mail);
        action.setCustomRule(customRule);
        customRule.getRuleActionList().add(action);
        CustomRule createdCustomRule = rulesService.createCustomRule(customRule, "", "");

        CustomRuleQuery query = RulesTestHelper.createBasicCustomRuleQuery();
        CustomRuleListCriteria criteria = new CustomRuleListCriteria();
        criteria.setKey(CustomRuleSearchKey.TICKET_ACTION_USER);
        criteria.setValue(mail);
        query.getCustomRuleSearchCriteria().add(criteria);

        CustomRuleListResponseDto customRulesResponse = validationService.getCustomRulesByQuery(query);
        List<CustomRule> customRules = customRulesResponse.getCustomRuleList();

        assertThat(customRules.size(), is(1));

        CustomRule fetchedCustomRule = customRules.get(0);
        assertThat(fetchedCustomRule.getGuid(), is(createdCustomRule.getGuid()));
        assertThat(fetchedCustomRule.getName(), is(createdCustomRule.getName()));
        assertThat(fetchedCustomRule.getUpdatedBy(), is(createdCustomRule.getUpdatedBy()));
    }

    @Test
    @OperateOnDeployment("normal")
    public void getCustomRuleListByQueryUserTest() throws Exception {
        CustomRule customRule = RulesTestHelper.createCompleteCustomRule();
        CustomRule createdCustomRule = rulesService.createCustomRule(customRule, "", "");

        CustomRuleQuery query = RulesTestHelper.createBasicCustomRuleQuery();
        CustomRuleListCriteria criteria = new CustomRuleListCriteria();
        criteria.setKey(CustomRuleSearchKey.RULE_USER);
        criteria.setValue(createdCustomRule.getUpdatedBy());
        query.getCustomRuleSearchCriteria().add(criteria);

        CustomRuleListResponseDto customRulesResponse = validationService.getCustomRulesByQuery(query);
        List<CustomRule> customRules = customRulesResponse.getCustomRuleList();

        assertTrue(customRules.size() > 0);

        assertTrue(customRules.stream()
                .anyMatch(r -> r.getGuid().equals(createdCustomRule.getGuid())));
    }

    @Test
    @OperateOnDeployment("normal")
    public void customRuleTriggeredLastTriggeredDateShouldBeSetTest() throws Exception {
        Calendar calendar = Calendar.getInstance();
        calendar.set(Calendar.MILLISECOND, 0);
        Date timestamp = calendar.getTime();

        CustomRule customRule = RulesTestHelper.createCompleteCustomRule();
        CustomRule createdCustomRule = rulesService.createCustomRule(customRule, "", "");

        MovementFact movementFact = RulesTestHelper.createBasicMovementFact();
        validationService.customRuleTriggered(createdCustomRule.getName(), createdCustomRule.getGuid(), movementFact, "EMAIL,test@test.com");

        CustomRule updatedCustomRule = rulesService.getCustomRuleByGuid(createdCustomRule.getGuid());
        String lastTriggered = DateUtils.dateToString(updatedCustomRule.getTriggered());
        assertThat(lastTriggered, is(notNullValue()));

        Date dateTriggered = DateUtils.stringToDate(lastTriggered);
        assertTrue(dateTriggered.getTime() >= timestamp.getTime());
    }

    @Test
    @OperateOnDeployment("normal")
    public void customRuleTriggeredNewTicketShouldBeCreatedTest() throws Exception {
        CustomRule customRule = RulesTestHelper.createCompleteCustomRule();
        CustomRule createdCustomRule = rulesService.createCustomRule(customRule, "", "");

        long openTicketsBefore = validationService.getNumberOfOpenTickets(createdCustomRule.getUpdatedBy());

        MovementFact movementFact = RulesTestHelper.createBasicMovementFact();
        validationService.customRuleTriggered(createdCustomRule.getName(), createdCustomRule.getGuid(), movementFact, "EMAIL,test@test.com");

        long openTicketsAfter = validationService.getNumberOfOpenTickets(createdCustomRule.getUpdatedBy());
        assertThat(openTicketsAfter, is(openTicketsBefore + 1));
    }

    @Test
    @OperateOnDeployment("normal")
    public void customRuleTriggeredSendToNAFTest() throws Exception {
        CustomRule customRule = RulesTestHelper.createCompleteCustomRule();
        CustomRule createdCustomRule = rulesService.createCustomRule(customRule, "", "");

        long openTicketsBefore = validationService.getNumberOfOpenTickets(createdCustomRule.getUpdatedBy());

        MovementFact movementFact = RulesTestHelper.createBasicMovementFact();
        validationService.customRuleTriggered(createdCustomRule.getName(), createdCustomRule.getGuid(), movementFact, "SEND_TO_NAF,SWE");

        long openTicketsAfter = validationService.getNumberOfOpenTickets(createdCustomRule.getUpdatedBy());
        assertThat(openTicketsAfter, is(openTicketsBefore + 1));
    }


}
