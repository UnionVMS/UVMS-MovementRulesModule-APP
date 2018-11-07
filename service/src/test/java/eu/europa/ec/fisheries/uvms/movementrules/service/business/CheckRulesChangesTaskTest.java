package eu.europa.ec.fisheries.uvms.movementrules.service.business;

import eu.europa.ec.fisheries.uvms.movementrules.service.RulesService;
import eu.europa.ec.fisheries.uvms.movementrules.service.RulesTestHelper;
import eu.europa.ec.fisheries.uvms.movementrules.service.TransactionalTests;
import eu.europa.ec.fisheries.uvms.movementrules.service.dao.RulesDao;
import eu.europa.ec.fisheries.uvms.movementrules.service.entity.CustomRule;
import eu.europa.ec.fisheries.uvms.movementrules.service.entity.Interval;
import eu.europa.ec.fisheries.uvms.movementrules.service.message.bean.ValidationServiceBean;
import org.jboss.arquillian.container.test.api.OperateOnDeployment;
import org.jboss.arquillian.junit.Arquillian;
import org.junit.Assert;
import org.junit.Test;
import org.junit.runner.RunWith;

import javax.inject.Inject;
import java.util.Date;


@RunWith(Arquillian.class)
public class CheckRulesChangesTaskTest extends TransactionalTests {

    @Inject
    RulesDao rulesDao;

    @Inject
    ValidationServiceBean validationService;

    @Inject
    RulesValidator rulesValidator;

    @Inject
    RulesService rulesService;


    @Test
@OperateOnDeployment("normal")
    public void checkRulesChangesTaskTest() throws Exception {
        CustomRule customRule = RulesTestHelper.createCompleteCustomRule();
        Interval interval = new Interval();
        interval.setCustomRule(customRule);
        interval.setStart(new Date(System.currentTimeMillis() - 20000));
        interval.setEnd(new Date(System.currentTimeMillis() - 10000));
        customRule.getIntervals().add(interval);
        customRule.setUpdated(new Date());
        customRule.setUpdatedBy("TestUser");

        customRule = rulesDao.createCustomRule(customRule);
        Assert.assertTrue(customRule.getActive());

        CheckRulesChangesTask checkRulesChangesTask = new CheckRulesChangesTask(validationService, rulesValidator, rulesService);
        checkRulesChangesTask.run();

        CustomRule createdCustomRule = rulesDao.getCustomRuleByGuid(customRule.getGuid());

        Assert.assertEquals(customRule.getGuid(), createdCustomRule.getGuid());
        Assert.assertFalse(createdCustomRule.getActive());
        Assert.assertTrue(createdCustomRule.getArchived());

    }
}
