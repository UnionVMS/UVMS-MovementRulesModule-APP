package eu.europa.ec.fisheries.uvms.movementrules.service.business;

import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.CoreMatchers.notNullValue;
import static org.hamcrest.CoreMatchers.nullValue;
import static org.junit.Assert.assertThat;
import static org.junit.Assert.assertTrue;
import java.util.Date;
import java.util.UUID;
import javax.inject.Inject;
import org.jboss.arquillian.container.test.api.OperateOnDeployment;
import org.jboss.arquillian.junit.Arquillian;
import org.junit.Before;
import org.junit.Test;
import org.junit.runner.RunWith;
import eu.europa.ec.fisheries.uvms.config.exception.ConfigServiceException;
import eu.europa.ec.fisheries.uvms.config.service.ParameterService;
import eu.europa.ec.fisheries.uvms.movementrules.service.TransactionalTests;
import eu.europa.ec.fisheries.uvms.movementrules.service.bean.RulesServiceBean;
import eu.europa.ec.fisheries.uvms.movementrules.service.config.ParameterKey;
import eu.europa.ec.fisheries.uvms.movementrules.service.constants.ServiceConstants;
import eu.europa.ec.fisheries.uvms.movementrules.service.dao.RulesDao;
import eu.europa.ec.fisheries.uvms.movementrules.service.entity.PreviousReport;
import eu.europa.ec.fisheries.uvms.movementrules.service.entity.Ticket;

@RunWith(Arquillian.class)
public class CheckCommunicationTaskTest extends TransactionalTests {

    private static final long ONE_HOUR_IN_MILLISECONDS = 3600000;
    
    @Inject
    RulesServiceBean rulesService;
    
    @Inject
    ParameterService parameterService;
    
    @Inject
    RulesDao rulesDao;
    
    @Before
    public void setThreshold() throws ConfigServiceException {
        parameterService.setStringValue(ParameterKey.ASSET_NOT_SENDING_THRESHOLD.getKey(), 
                String.valueOf(ONE_HOUR_IN_MILLISECONDS), "");
    }
    
    @Test
@OperateOnDeployment ("normal")
    public void runTaskWithValidReport() throws Exception {
        PreviousReport previousReport = getBasicPreviousReport();
        rulesDao.updatePreviousReport(previousReport);
        
        String assetGuid = previousReport.getAssetGuid();
        
        new CheckCommunicationTask(rulesService, parameterService).run();
        
        Ticket ticket = rulesDao.getTicketByAssetAndRule(assetGuid, ServiceConstants.ASSET_NOT_SENDING_RULE);
        assertThat(ticket, is(nullValue()));
    }
    
    @Test
@OperateOnDeployment("normal")
    public void runWithThresholdPassed() throws Exception {
        PreviousReport previousReport = getBasicPreviousReport();
        previousReport.setPositionTime(new Date(System.currentTimeMillis() - ONE_HOUR_IN_MILLISECONDS));
        rulesDao.updatePreviousReport(previousReport);
        
        String assetGuid = previousReport.getAssetGuid();
        
        new CheckCommunicationTask(rulesService, parameterService).run();
        
        Ticket ticket = rulesDao.getTicketByAssetAndRule(assetGuid, ServiceConstants.ASSET_NOT_SENDING_RULE);
        assertThat(ticket, is(notNullValue()));
        assertThat(ticket.getTicketCount(), is(1L));
    }
    
    @Test
@OperateOnDeployment ("normal")
    public void runTwiceWithThresholdPassed() throws Exception {
        PreviousReport previousReport = getBasicPreviousReport();
        previousReport.setPositionTime(new Date(System.currentTimeMillis() - ONE_HOUR_IN_MILLISECONDS));
        rulesDao.updatePreviousReport(previousReport);
        
        String assetGuid = previousReport.getAssetGuid();
        
        CheckCommunicationTask checkCommunicationTask = new CheckCommunicationTask(rulesService, parameterService);
        checkCommunicationTask.run();
        checkCommunicationTask.run();
        
        Ticket ticket = rulesDao.getTicketByAssetAndRule(assetGuid, ServiceConstants.ASSET_NOT_SENDING_RULE);
        assertThat(ticket, is(notNullValue()));
        assertThat(ticket.getTicketCount(), is(1L));
    }
    
    @Test
@OperateOnDeployment ("normal")
    public void updateThresholdPassed() throws Exception {
        PreviousReport previousReport = getBasicPreviousReport();
        previousReport.setPositionTime(new Date(System.currentTimeMillis() - ONE_HOUR_IN_MILLISECONDS));
        rulesDao.updatePreviousReport(previousReport);
        
        String assetGuid = previousReport.getAssetGuid();
        
        CheckCommunicationTask checkCommunicationTask = new CheckCommunicationTask(rulesService, parameterService);
        checkCommunicationTask.run();
        
        previousReport = rulesDao.getPreviousReportByAssetGuid(previousReport.getAssetGuid());
        previousReport.setPositionTime(new Date(System.currentTimeMillis() - 3*ONE_HOUR_IN_MILLISECONDS));
        previousReport.setUpdated(new Date(System.currentTimeMillis() - 2*ONE_HOUR_IN_MILLISECONDS));
        rulesDao.updatePreviousReport(previousReport);
        
        checkCommunicationTask.run();
        
        Ticket ticket = rulesDao.getTicketByAssetAndRule(assetGuid, ServiceConstants.ASSET_NOT_SENDING_RULE);
        assertThat(ticket, is(notNullValue()));
        assertThat(ticket.getTicketCount(), is(2L));
    }
    
    @Test
@OperateOnDeployment ("normal")
    public void checkPreviousReportUpdateTimeUpdated() throws Exception {
        PreviousReport previousReport = getBasicPreviousReport();
        previousReport.setPositionTime(new Date(System.currentTimeMillis() - ONE_HOUR_IN_MILLISECONDS));
        rulesDao.updatePreviousReport(previousReport);
      
        CheckCommunicationTask checkCommunicationTask = new CheckCommunicationTask(rulesService, parameterService);
        checkCommunicationTask.run();
      
        PreviousReport fetchedReport = rulesDao.getPreviousReportByAssetGuid(previousReport.getAssetGuid());
        
        assertTrue(fetchedReport.getUpdated().after(previousReport.getUpdated()));
    }
    
    @Test
@OperateOnDeployment ("normal")
    public void runTaskWith30MinSteps() throws Exception {
        long thirtyMinsInMs = ONE_HOUR_IN_MILLISECONDS / 2;
        PreviousReport previousReport = getBasicPreviousReport();
        rulesDao.updatePreviousReport(previousReport);
        
        String assetGuid = previousReport.getAssetGuid();
        
        CheckCommunicationTask checkCommunicationTask = new CheckCommunicationTask(rulesService, parameterService);
        checkCommunicationTask.run();

        Ticket ticket = rulesDao.getTicketByAssetAndRule(assetGuid, ServiceConstants.ASSET_NOT_SENDING_RULE);
        assertThat(ticket, is(nullValue()));
        
        // 30 mins
        previousReport = rulesDao.getPreviousReportByAssetGuid(previousReport.getAssetGuid());
        Date positionTime = previousReport.getPositionTime();
        Date updated = previousReport.getUpdated();
        previousReport.setPositionTime(new Date(positionTime.getTime() - thirtyMinsInMs));
        previousReport.setUpdated(new Date(updated.getTime() - thirtyMinsInMs));
        rulesDao.updatePreviousReport(previousReport);
        
        checkCommunicationTask.run();
        
        ticket = rulesDao.getTicketByAssetAndRule(assetGuid, ServiceConstants.ASSET_NOT_SENDING_RULE);
        assertThat(ticket, is(nullValue()));
        
        // 1 hour
        previousReport = rulesDao.getPreviousReportByAssetGuid(previousReport.getAssetGuid());
        positionTime = previousReport.getPositionTime();
        updated = previousReport.getUpdated();
        previousReport.setPositionTime(new Date(positionTime.getTime() - thirtyMinsInMs));
        previousReport.setUpdated(new Date(updated.getTime() - thirtyMinsInMs));
        rulesDao.updatePreviousReport(previousReport);
        
        checkCommunicationTask.run();
        
        ticket = rulesDao.getTicketByAssetAndRule(assetGuid, ServiceConstants.ASSET_NOT_SENDING_RULE);
        assertThat(ticket, is(notNullValue()));
        assertThat(ticket.getTicketCount(), is(1L));

        // 1.5 hour
        previousReport = rulesDao.getPreviousReportByAssetGuid(previousReport.getAssetGuid());
        positionTime = previousReport.getPositionTime();
        updated = previousReport.getUpdated();
        previousReport.setPositionTime(new Date(positionTime.getTime() - thirtyMinsInMs));
        previousReport.setUpdated(new Date(updated.getTime() - thirtyMinsInMs));
        rulesDao.updatePreviousReport(previousReport);
        
        checkCommunicationTask.run();
        
        ticket = rulesDao.getTicketByAssetAndRule(assetGuid, ServiceConstants.ASSET_NOT_SENDING_RULE);
        assertThat(ticket, is(notNullValue()));
        assertThat(ticket.getTicketCount(), is(1L));
        
        // 2 hours
        previousReport = rulesDao.getPreviousReportByAssetGuid(previousReport.getAssetGuid());
        positionTime = previousReport.getPositionTime();
        updated = previousReport.getUpdated();
        previousReport.setPositionTime(new Date(positionTime.getTime() - thirtyMinsInMs));
        previousReport.setUpdated(new Date(updated.getTime() - thirtyMinsInMs));
        rulesDao.updatePreviousReport(previousReport);
        
        checkCommunicationTask.run();
        
        ticket = rulesDao.getTicketByAssetAndRule(assetGuid, ServiceConstants.ASSET_NOT_SENDING_RULE);
        assertThat(ticket, is(notNullValue()));
        assertThat(ticket.getTicketCount(), is(2L));
        
        // 2.5 hours
        previousReport = rulesDao.getPreviousReportByAssetGuid(previousReport.getAssetGuid());
        positionTime = previousReport.getPositionTime();
        updated = previousReport.getUpdated();
        previousReport.setPositionTime(new Date(positionTime.getTime() - thirtyMinsInMs));
        previousReport.setUpdated(new Date(updated.getTime() - thirtyMinsInMs));
        rulesDao.updatePreviousReport(previousReport);
        
        checkCommunicationTask.run();
        
        ticket = rulesDao.getTicketByAssetAndRule(assetGuid, ServiceConstants.ASSET_NOT_SENDING_RULE);
        assertThat(ticket, is(notNullValue()));
        assertThat(ticket.getTicketCount(), is(2L));
    }
    
    @Test
@OperateOnDeployment ("normal")
    public void runTaskWith30MinStepsWithPastPositionTime() throws Exception {
        long thirtyMinsInMs = ONE_HOUR_IN_MILLISECONDS / 2;
        PreviousReport previousReport = getBasicPreviousReport();
        previousReport.setPositionTime(new Date(System.currentTimeMillis() - thirtyMinsInMs));
        rulesDao.updatePreviousReport(previousReport);
        
        String assetGuid = previousReport.getAssetGuid();
        
        CheckCommunicationTask checkCommunicationTask = new CheckCommunicationTask(rulesService, parameterService);
        checkCommunicationTask.run();

        Ticket ticket = rulesDao.getTicketByAssetAndRule(assetGuid, ServiceConstants.ASSET_NOT_SENDING_RULE);
        assertThat(ticket, is(nullValue()));
        
        // 1 hour
        previousReport = rulesDao.getPreviousReportByAssetGuid(previousReport.getAssetGuid());
        Date positionTime = previousReport.getPositionTime();
        Date updated = previousReport.getUpdated();
        previousReport.setPositionTime(new Date(positionTime.getTime() - thirtyMinsInMs));
        previousReport.setUpdated(new Date(updated.getTime() - thirtyMinsInMs));
        rulesDao.updatePreviousReport(previousReport);
        
        checkCommunicationTask.run();
        
        ticket = rulesDao.getTicketByAssetAndRule(assetGuid, ServiceConstants.ASSET_NOT_SENDING_RULE);
        assertThat(ticket, is(notNullValue()));
        assertThat(ticket.getTicketCount(), is(1L));

        // 1.5 hour
        previousReport = rulesDao.getPreviousReportByAssetGuid(previousReport.getAssetGuid());
        positionTime = previousReport.getPositionTime();
        updated = previousReport.getUpdated();
        previousReport.setPositionTime(new Date(positionTime.getTime() - thirtyMinsInMs));
        previousReport.setUpdated(new Date(updated.getTime() - thirtyMinsInMs));
        rulesDao.updatePreviousReport(previousReport);
        
        checkCommunicationTask.run();
        
        ticket = rulesDao.getTicketByAssetAndRule(assetGuid, ServiceConstants.ASSET_NOT_SENDING_RULE);
        assertThat(ticket, is(notNullValue()));
        assertThat(ticket.getTicketCount(), is(1L));

        // 2 hour
        previousReport = rulesDao.getPreviousReportByAssetGuid(previousReport.getAssetGuid());
        positionTime = previousReport.getPositionTime();
        updated = previousReport.getUpdated();
        previousReport.setPositionTime(new Date(positionTime.getTime() - thirtyMinsInMs));
        previousReport.setUpdated(new Date(updated.getTime() - thirtyMinsInMs));
        rulesDao.updatePreviousReport(previousReport);
        
        checkCommunicationTask.run();
        
        ticket = rulesDao.getTicketByAssetAndRule(assetGuid, ServiceConstants.ASSET_NOT_SENDING_RULE);
        assertThat(ticket, is(notNullValue()));
        assertThat(ticket.getTicketCount(), is(2L));
        
        // 2.5 hours
        previousReport = rulesDao.getPreviousReportByAssetGuid(previousReport.getAssetGuid());
        positionTime = previousReport.getPositionTime();
        updated = previousReport.getUpdated();
        previousReport.setPositionTime(new Date(positionTime.getTime() - thirtyMinsInMs));
        previousReport.setUpdated(new Date(updated.getTime() - thirtyMinsInMs));
        rulesDao.updatePreviousReport(previousReport);
        
        checkCommunicationTask.run();
        
        ticket = rulesDao.getTicketByAssetAndRule(assetGuid, ServiceConstants.ASSET_NOT_SENDING_RULE);
        assertThat(ticket, is(notNullValue()));
        assertThat(ticket.getTicketCount(), is(2L));
        
        // 3 hours
        previousReport = rulesDao.getPreviousReportByAssetGuid(previousReport.getAssetGuid());
        positionTime = previousReport.getPositionTime();
        updated = previousReport.getUpdated();
        previousReport.setPositionTime(new Date(positionTime.getTime() - thirtyMinsInMs));
        previousReport.setUpdated(new Date(updated.getTime() - thirtyMinsInMs));
        rulesDao.updatePreviousReport(previousReport);
        
        checkCommunicationTask.run();
        
        ticket = rulesDao.getTicketByAssetAndRule(assetGuid, ServiceConstants.ASSET_NOT_SENDING_RULE);
        assertThat(ticket, is(notNullValue()));
        assertThat(ticket.getTicketCount(), is(3L));
    }

    
    
    private PreviousReport getBasicPreviousReport() {
        PreviousReport previousReport = new PreviousReport();
        previousReport.setPositionTime(new Date());
        previousReport.setAssetGuid(UUID.randomUUID().toString());
        previousReport.setUpdated(new Date());
        previousReport.setUpdatedBy("UVMS");
        return previousReport;
    }
}
