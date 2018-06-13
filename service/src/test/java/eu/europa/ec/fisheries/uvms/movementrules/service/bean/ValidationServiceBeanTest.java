package eu.europa.ec.fisheries.uvms.movementrules.service.bean;

import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.CoreMatchers.notNullValue;
import static org.junit.Assert.assertThat;
import static org.junit.Assert.assertTrue;
import java.util.Calendar;
import java.util.Date;
import java.util.List;
import javax.inject.Inject;
import org.jboss.arquillian.junit.Arquillian;
import org.junit.Test;
import org.junit.runner.RunWith;
import eu.europa.ec.fisheries.schema.movementrules.customrule.v1.CustomRuleType;
import eu.europa.ec.fisheries.schema.movementrules.module.v1.GetCustomRuleListByQueryResponse;
import eu.europa.ec.fisheries.schema.movementrules.search.v1.CustomRuleListCriteria;
import eu.europa.ec.fisheries.schema.movementrules.search.v1.CustomRuleQuery;
import eu.europa.ec.fisheries.schema.movementrules.search.v1.CustomRuleSearchKey;
import eu.europa.ec.fisheries.uvms.commons.date.DateUtils;
import eu.europa.ec.fisheries.uvms.movementrules.service.RulesService;
import eu.europa.ec.fisheries.uvms.movementrules.service.RulesTestHelper;
import eu.europa.ec.fisheries.uvms.movementrules.service.TransactionalTests;
import eu.europa.ec.fisheries.uvms.movementrules.service.ValidationService;
import eu.europa.ec.fisheries.uvms.movementrules.service.business.MovementFact;
import eu.europa.ec.fisheries.uvms.movementrules.service.business.RawMovementFact;
import eu.europa.ec.fisheries.uvms.movementrules.service.dto.CustomRuleListResponseDto;
import eu.europa.ec.fisheries.uvms.movementrules.service.entity.CustomRule;
import eu.europa.ec.fisheries.uvms.movementrules.service.entity.SanityRule;

@RunWith(Arquillian.class)
public class ValidationServiceBeanTest extends TransactionalTests {

    @Inject
    ValidationService validationService;
    
    @Inject
    RulesService rulesService;
    
    @Test
    public void getCustomRulesByUserTest() throws Exception {
        CustomRule customRule = RulesTestHelper.createCompleteCustomRule();
        CustomRule createdCustomRule = rulesService.createCustomRule(customRule, "", "");
        
        List<CustomRule> customRulesByUser = validationService.getCustomRulesByUser(createdCustomRule.getUpdatedBy());
        assertTrue(customRulesByUser.size() > 0);
    }
    
    @Test
    public void getRunnableCustomRulesTest() throws Exception {
        List<CustomRule> runnableCustomRulesBefore = validationService.getRunnableCustomRules();
        CustomRule customRule = RulesTestHelper.createCompleteCustomRule();
        rulesService.createCustomRule(customRule, "", "");
        
        List<CustomRule> runnableCustomRulesAfter = validationService.getRunnableCustomRules();
        assertThat(runnableCustomRulesAfter.size(), is(runnableCustomRulesBefore.size() + 1));
    }
    
    @Test
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
    public void getSanityRulesTest() throws Exception {
        List<SanityRule> sanityRules = validationService.getSanityRules();
        assertTrue(sanityRules.size() > 0);
    }
    
    @Test
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
    public void customRuleTriggeredSendToNAFTest() throws Exception {
        CustomRule customRule = RulesTestHelper.createCompleteCustomRule();
        CustomRule createdCustomRule = rulesService.createCustomRule(customRule, "", "");
        
        long openTicketsBefore = validationService.getNumberOfOpenTickets(createdCustomRule.getUpdatedBy());
        
        MovementFact movementFact = RulesTestHelper.createBasicMovementFact();
        validationService.customRuleTriggered(createdCustomRule.getName(), createdCustomRule.getGuid(), movementFact, "SEND_TO_NAF,SWE");
        
        long openTicketsAfter = validationService.getNumberOfOpenTickets(createdCustomRule.getUpdatedBy());
        assertThat(openTicketsAfter, is(openTicketsBefore + 1));
    }
    
    @Test
    public void createAlarmReportNewAlarmReportShouldBeCreatedTest() throws Exception {
        long alarmReportsBefore = validationService.getNumberOfOpenAlarmReports();
        
        RawMovementFact rawMovementFact = RulesTestHelper.createBasicRawMovementFact();
        validationService.createAlarmReport("Test Rule", rawMovementFact);
        
        long alarmReportsAfter = validationService.getNumberOfOpenAlarmReports();
        assertThat(alarmReportsAfter, is(alarmReportsBefore + 1));
    }
    
}
