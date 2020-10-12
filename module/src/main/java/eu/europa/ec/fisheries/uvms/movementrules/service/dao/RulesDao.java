/*
﻿Developed with the contribution of the European Commission - Directorate General for Maritime Affairs and Fisheries
© European Union, 2015-2016.

This file is part of the Integrated Fisheries Data Management (IFDM) Suite. The IFDM Suite is free software: you can
redistribute it and/or modify it under the terms of the GNU General Public License as published by the
Free Software Foundation, either version 3 of the License, or any later version. The IFDM Suite is distributed in
the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more details. You should have received a
copy of the GNU General Public License along with the IFDM Suite. If not, see <http://www.gnu.org/licenses/>.
 */
package eu.europa.ec.fisheries.uvms.movementrules.service.dao;

import java.time.Instant;
import java.util.List;
import java.util.UUID;
import javax.ejb.Stateless;
import javax.persistence.EntityManager;
import javax.persistence.NoResultException;
import javax.persistence.PersistenceContext;
import javax.persistence.TypedQuery;

import eu.europa.ec.fisheries.uvms.movementrules.service.constants.ServiceConstants;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import eu.europa.ec.fisheries.uvms.movementrules.service.entity.CustomRule;
import eu.europa.ec.fisheries.uvms.movementrules.service.entity.PreviousReport;
import eu.europa.ec.fisheries.uvms.movementrules.service.entity.RuleSubscription;
import eu.europa.ec.fisheries.uvms.movementrules.service.entity.Ticket;

@Stateless
public class RulesDao {

    private static final Logger LOG = LoggerFactory.getLogger(RulesDao.class);
    
    @PersistenceContext
    private EntityManager em;

    public void flush() {
        em.flush();
    }

    public CustomRule createCustomRule(CustomRule entity){
        em.persist(entity);
        return entity;
    }

    public CustomRule getCustomRuleByGuid(UUID guid) {
            return em.find(CustomRule.class, guid);
    }

    public void removeCustomRuleAfterTests(CustomRule customRule) {
        em.remove(em.contains(customRule) ? customRule : em.merge(customRule));
    }

    public void removeTicketAfterTests(Ticket ticket) {
        em.remove(em.contains(ticket) ? ticket : em.merge(ticket));
    }

    public Ticket getTicketByGuid(UUID guid){
        return em.find(Ticket.class, guid);
    }

    public List<Ticket> getTicketsByMovements(List<String> movements) {
        TypedQuery<Ticket> query = em.createNamedQuery(Ticket.FIND_TICKETS_BY_MOVEMENTS, Ticket.class);
        query.setParameter("movements", movements);
        return query.getResultList();
    }

    public long countTicketListByMovements(List<String> movements) {
        TypedQuery<Long> query = em.createNamedQuery(Ticket.COUNT_TICKETS_BY_MOVEMENTS, Long.class);
        query.setParameter("movements", movements);
        return query.getSingleResult();
    }

    public List<UUID> getCustomRulesForTicketsByUser(String owner) {
        TypedQuery<UUID> query = em.createNamedQuery(CustomRule.FIND_CUSTOM_RULE_GUID_FOR_TICKETS, UUID.class);
        query.setParameter("owner", owner);
        return query.getResultList();
    }


    public long getNumberOfOpenTickets(List<String> validRuleGuids){
        try {
            TypedQuery<Long> query = em.createNamedQuery(Ticket.COUNT_OPEN_TICKETS, Long.class);
            query.setParameter("validRuleGuids", validRuleGuids);
            return query.getSingleResult();
        } catch (NoResultException e) {
            return 0;
        }
    }

    public Ticket getLatestTicketForRule(UUID ruleGuid){
        try {
            TypedQuery<Ticket> query = em.createNamedQuery(Ticket.FIND_LATEST_TICKET_FOR_RULE, Ticket.class);
            query.setParameter("ruleGuid", ruleGuid.toString());
            return query.setMaxResults(1).getSingleResult();
        } catch (NoResultException e) {
            return null;
        }
    }


    public CustomRule updateCustomRule(CustomRule entity) {
        return em.merge(entity);
    }

    public void removeSubscription(RuleSubscription entity) {
        em.remove(entity);
    }

    public void detachSubscription(RuleSubscription subscription) {
        em.detach(subscription);
        subscription.setId(null);
    }

    public Ticket updateTicket(Ticket entity) {
        return em.merge(entity);
    }


    public List<CustomRule> getRunnableCustomRuleList() {
        TypedQuery<CustomRule> query = em.createNamedQuery(CustomRule.GET_RUNNABLE_CUSTOM_RULES, CustomRule.class);
        return query.getResultList();
    }


    public List<CustomRule> getCustomRulesByUser(String updatedBy) {
        TypedQuery<CustomRule> query = em.createNamedQuery(CustomRule.LIST_CUSTOM_RULES_BY_USER, CustomRule.class);
        query.setParameter("updatedBy", updatedBy);
        return query.getResultList();
    }



    public Ticket createTicket(Ticket ticket) {
        em.persist(ticket);
        return ticket;
    }


    public Long getCustomRuleListSearchCount(String countSql) {
        LOG.debug("CUSTOM RULE SQL QUERY IN LIST COUNT: {}", countSql);

        TypedQuery<Long> query = em.createQuery(countSql, Long.class);
        return query.getSingleResult();
    }

    public List<CustomRule> getCustomRuleListPaginated(Integer page, Integer listSize, String sql) {
        LOG.debug("CUSTOM RULE SQL QUERY IN LIST PAGINATED: {}", sql);

        TypedQuery<CustomRule> query = em.createQuery(sql, CustomRule.class);
        query.setFirstResult(listSize * (page - 1));
        query.setMaxResults(listSize);
        return query.getResultList();
    }

    public Long getAlarmListSearchCount(String countSql) {
        LOG.debug("ALARM SQL QUERY IN LIST COUNT: {}", countSql);

        TypedQuery<Long> query = em.createQuery(countSql, Long.class);
        return query.getSingleResult();
    }


    public Long getTicketListSearchCount(String countSql) {
        LOG.debug("TICKET SQL QUERY IN LIST COUNT: {}", countSql);

        TypedQuery<Long> query = em.createQuery(countSql, Long.class);
        return query.getSingleResult();
    }

    public List<Ticket> getTicketListPaginated(Integer page, Integer listSize, String sql) {
        LOG.debug("TICKET SQL QUERY IN LIST PAGINATED: {}", sql);

        TypedQuery<Ticket> query = em.createQuery(sql, Ticket.class);
        query.setFirstResult(listSize * (page - 1));
        query.setMaxResults(listSize);
        return query.getResultList();
    }

    public List<Ticket> getTicketList(String sql) {
        LOG.debug("TICKET SQL QUERY IN LIST: {}", sql);

        TypedQuery<Ticket> query = em.createQuery(sql, Ticket.class);
        return query.getResultList();
    }

    public List<PreviousReport> getPreviousReportList() {
        TypedQuery<PreviousReport> query = em.createNamedQuery(PreviousReport.GET_ALL_PREVIOUS_REPORTS, PreviousReport.class);
        return query.getResultList();
    }

    // Used by timer to prevent duplicate tickets for passing the reporting
    // deadline
    public Ticket getTicketByAssetAndRule(String assetGuid, String ruleGuid) {
        try {
            TypedQuery<Ticket> query = em.createNamedQuery(Ticket.FIND_TICKET_BY_ASSET_AND_RULE, Ticket.class);
            query.setParameter("assetGuid", assetGuid);
            query.setParameter("ruleGuid", ruleGuid);
            return query.getSingleResult();
        } catch (NoResultException e) {
            return null;
        }
    }


    public PreviousReport getPreviousReportByAssetGuid(String assetGuid) {
        try {
            TypedQuery<PreviousReport> query = em.createNamedQuery(PreviousReport.FIND_PREVIOUS_REPORT_BY_ASSET_GUID, PreviousReport.class);
            query.setParameter("assetGuid", assetGuid);
            return query.getSingleResult();
        } catch (NoResultException e) {
            return null;
        }
    }

    public PreviousReport updatePreviousReport(PreviousReport report) {
        return em.merge(report);
    }

    public void deletePreviousReport(PreviousReport report) {
        if(report != null) {
            em.remove(report);
        }
    }

    public long getNumberOfTicketsForRule(String ruleGuid){
        TypedQuery<Long> query = em.createNamedQuery(Ticket.COUNT_TICKETS_FOR_RULE, Long.class);
        query.setParameter("ruleGuid", ruleGuid);
        return query.getSingleResult();
    }

    public List<Ticket> getAssetNotSendingTicketsBetween(Instant from, Instant to) {
        TypedQuery<Ticket> query = em.createNamedQuery(Ticket.FIND_ALL_ASSET_NOT_SENDING_TICKETS_BETWEEN, Ticket.class);
        query.setParameter("from", from);
        query.setParameter("to", to);
        return query.getResultList();
    }
}