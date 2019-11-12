package eu.europa.ec.fisheries.uvms.movementrules.service.dao;


import eu.europa.ec.fisheries.schema.movementrules.ticket.v1.TicketStatusType;
import eu.europa.ec.fisheries.uvms.movementrules.service.RulesTestHelper;
import eu.europa.ec.fisheries.uvms.movementrules.service.TransactionalTests;
import eu.europa.ec.fisheries.uvms.movementrules.service.constants.ServiceConstants;
import eu.europa.ec.fisheries.uvms.movementrules.service.entity.PreviousReport;
import eu.europa.ec.fisheries.uvms.movementrules.service.entity.Ticket;
import org.jboss.arquillian.container.test.api.OperateOnDeployment;
import org.jboss.arquillian.junit.Arquillian;
import org.junit.Test;
import org.junit.runner.RunWith;

import javax.inject.Inject;
import java.time.Instant;
import java.util.List;
import java.util.UUID;

import static org.junit.Assert.*;

@RunWith(Arquillian.class)
public class RulesDaoTest extends TransactionalTests {

    @Inject
    RulesDao dao;

    @Test
    @OperateOnDeployment("normal")
    public void deletePreviousReportTest(){
        UUID assetGuid = UUID.randomUUID();
        PreviousReport report = new PreviousReport();
        report.setPositionTime(Instant.now());
        report.setAssetGuid(assetGuid.toString());
        report.setUpdated(Instant.now());
        report.setUpdatedBy("UVMS");
        report = dao.updatePreviousReport(report);

        assertNotNull(report.getId());

        dao.deletePreviousReport(report);
        assertNull(dao.getPreviousReportByAssetGuid(assetGuid.toString()));
    }

    @Test
    @OperateOnDeployment("normal")
    public void getAssetNotSendingTicketsTest(){
        Instant from = Instant.now();
        Ticket ticket = createBasicTicket();
        ticket = dao.createTicket(ticket);
        assertNotNull(ticket.getGuid());
        UUID ticketGuid = ticket.getGuid();

        List<Ticket> assetNotSendingTickets = dao.getAssetNotSendingTicketsBetween(from, Instant.now());
        assertTrue(assetNotSendingTickets.stream().anyMatch(t -> t.getGuid().equals(ticketGuid)));
        assertTrue(assetNotSendingTickets.stream().allMatch(t -> t.getRuleGuid().equals(ServiceConstants.ASSET_NOT_SENDING_RULE)));
    }

    private Ticket createBasicTicket(){
        Ticket ticket = new Ticket();
        ticket.setAssetGuid(UUID.randomUUID().toString());
        ticket.setCreatedDate(Instant.now());
        ticket.setRuleName(ServiceConstants.ASSET_NOT_SENDING_RULE);
        ticket.setRuleGuid(ServiceConstants.ASSET_NOT_SENDING_RULE);
        ticket.setUpdatedBy("The Tester");
        ticket.setUpdated(Instant.now());
        ticket.setStatus(TicketStatusType.POLL_PENDING);
        ticket.setTicketCount(1L);

        return ticket;
    }
}
