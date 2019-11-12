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
package eu.europa.ec.fisheries.uvms.movementrules.rest.service;

import eu.europa.ec.fisheries.schema.movementrules.module.v1.GetTicketListByMovementsResponse;
import eu.europa.ec.fisheries.schema.movementrules.module.v1.GetTicketListByQueryResponse;
import eu.europa.ec.fisheries.schema.movementrules.search.v1.TicketQuery;
import eu.europa.ec.fisheries.schema.movementrules.ticket.v1.TicketStatusType;
import eu.europa.ec.fisheries.schema.movementrules.ticket.v1.TicketType;
import eu.europa.ec.fisheries.uvms.commons.date.DateUtils;
import eu.europa.ec.fisheries.uvms.movementrules.rest.error.ErrorHandler;
import eu.europa.ec.fisheries.uvms.movementrules.service.bean.RulesServiceBean;
import eu.europa.ec.fisheries.uvms.movementrules.service.bean.ValidationServiceBean;
import eu.europa.ec.fisheries.uvms.movementrules.service.business.MRDateUtils;
import eu.europa.ec.fisheries.uvms.movementrules.service.dao.RulesDao;
import eu.europa.ec.fisheries.uvms.movementrules.service.dto.TicketListResponseDto;
import eu.europa.ec.fisheries.uvms.movementrules.service.entity.Ticket;
import eu.europa.ec.fisheries.uvms.movementrules.service.mapper.TicketMapper;
import eu.europa.ec.fisheries.uvms.rest.security.RequiresFeature;
import eu.europa.ec.fisheries.uvms.rest.security.UnionVMSFeature;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import javax.ejb.EJB;
import javax.ejb.Stateless;
import javax.inject.Inject;
import javax.ws.rs.*;
import javax.ws.rs.core.MediaType;
import javax.ws.rs.core.Response;
import java.time.Instant;
import java.time.temporal.ChronoUnit;
import java.util.List;
import java.util.UUID;

@Path("/tickets")
@Stateless
@Consumes(value = { MediaType.APPLICATION_JSON })
@Produces(value = { MediaType.APPLICATION_JSON })
public class TicketRestResource {

    private static final Logger LOG = LoggerFactory.getLogger(TicketRestResource.class);

    @Inject
    RulesDao rulesDao;

    @EJB
    private RulesServiceBean rulesService;

    @EJB
    private ValidationServiceBean validationService;

    @POST
    @Path("/list/{loggedInUser}")
    @RequiresFeature(UnionVMSFeature.viewAlarmsOpenTickets)
    public Response getTicketListByQuery(@PathParam("loggedInUser") String loggedInUser, TicketQuery query) {
        LOG.info("Get tickets list invoked in rest layer");
        try {
            TicketListResponseDto ticketList = rulesService.getTicketList(loggedInUser, query);
            GetTicketListByQueryResponse response = new GetTicketListByQueryResponse();
            response.setCurrentPage(ticketList.getCurrentPage());
            response.setTotalNumberOfPages(ticketList.getTotalNumberOfPages());
            List<TicketType> ticketTypes = TicketMapper.listToTicketType(ticketList.getTicketList());
            response.getTickets().addAll(ticketTypes);
            return Response.ok(response).build();
        } catch (Exception ex) {
            LOG.error("[ERROR] Error when getting ticket list by query ] {} ", ex.getMessage());
            return ErrorHandler.getFault(ex);
        }
    }

    @POST
    @Path("/listByMovements")
    @RequiresFeature(UnionVMSFeature.viewAlarmsOpenTickets)
    public Response getTicketsByMovementGuidList(List<String> movements) {
        LOG.info("Get tickets by movements invoked in rest layer");
        try {
            List<Ticket> tickets = rulesService.getTicketsByMovements(movements);
            GetTicketListByMovementsResponse response = new GetTicketListByMovementsResponse();
            List<TicketType> ticketTypes = TicketMapper.listToTicketType(tickets);
            response.getTickets().addAll(ticketTypes);
            return Response.ok(response).build();
        } catch (Exception ex) {
            LOG.error("[ Error when getting ticket list by movements. ] {} ", ex);
            return ErrorHandler.getFault(ex);
        }
    }

    @POST
    @Path("/countByMovements")
    @RequiresFeature(UnionVMSFeature.viewAlarmsOpenTickets)
    public Response countTicketsByMovementGuidList(List<String> movements) {
        try {
            Long count = rulesService.countTicketsByMovements(movements);
            return Response.ok(count).build();
        } catch (Exception e) {
            LOG.error("[ Error when counting number of open tickets by movements. ] {} ", e);
            return ErrorHandler.getFault(e);
        }
    }

    @PUT
    @Path("/status")
    @RequiresFeature(UnionVMSFeature.manageAlarmsOpenTickets)
    public Response updateTicketStatus(final TicketType ticketType) {
        LOG.info("Update ticket status invoked in rest layer");
        try {
            Ticket entity = TicketMapper.toTicketEntity(ticketType);
            entity = rulesService.updateTicketStatus(entity);
            TicketType response = TicketMapper.toTicketType(entity);
            return Response.ok(response).build();
        } catch (Exception e) {
            LOG.error("[ Error when updating ticket. ] {} ", e);
            return ErrorHandler.getFault(e);
        }
    }

    @POST
    @Path("/status/{loggedInUser}/{status}")
    @RequiresFeature(UnionVMSFeature.manageAlarmsOpenTickets)
    public Response updateTicketStatusByQuery(@PathParam("loggedInUser") String loggedInUser, TicketQuery query,
                                              @PathParam("status") TicketStatusType status) {
        LOG.info("Update ticket status invoked in rest layer");
        try {
            List<Ticket> ticketList = rulesService.updateTicketStatusByQuery(loggedInUser, query, status);
            List<TicketType> response = TicketMapper.listToTicketType(ticketList);
            return Response.ok(response).build();
        } catch (Exception e) {
            LOG.error("[ Error when updating tickets. ] {} ", e);
            return ErrorHandler.getFault(e);
        }
    }

    @GET
    @Path("/{guid}")
    @RequiresFeature(UnionVMSFeature.viewAlarmsOpenTickets)
    public Response getTicketByGuid(@PathParam("guid") String guid) {
        try {
            Ticket ticket = rulesService.getTicketByGuid(UUID.fromString(guid));
            TicketType response = TicketMapper.toTicketType(ticket);
            return Response.ok(response).build();
        } catch (Exception e) {
            LOG.error("[ Error when getting ticket by GUID. ] {} ", e);
            return ErrorHandler.getFault(e);
        }
    }

    @GET
    @Path("/countopen/{loggedInUser}")
    @RequiresFeature(UnionVMSFeature.viewAlarmsOpenTickets)
    public Response getNumberOfOpenTicketReports(@PathParam(value = "loggedInUser") final String loggedInUser) {
        try {
            Long count = validationService.getNumberOfOpenTickets(loggedInUser);
            return Response.ok(count).build();
        } catch (Exception e) {
            LOG.error("[ Error when getting number of open tickets. ] {} ", e);
            return ErrorHandler.getFault(e);
        }
    }

    @GET
    @Path("/countAssetsNotSending")
    @RequiresFeature(UnionVMSFeature.viewAlarmsOpenTickets)
    public Response getNumberOfAssetsNotSending() {
        try {
            Long count = rulesService.getNumberOfAssetsNotSending();
            return Response.ok(count).build();
        } catch (Exception e) {
            LOG.error("[ Error when getting number of assets not sending. ] {} ", e);
            return ErrorHandler.getFault(e);
        }
    }

    @GET
    @Path("/AssetsNotSending")
    @RequiresFeature(UnionVMSFeature.viewAlarmsOpenTickets)
    public Response getAssetNotSEndingTicketsBetween(@QueryParam("fromDate") String fromString, @QueryParam("toDate") String toString) {
        try {
            Instant from = (fromString == null || fromString.isEmpty() ? Instant.now().minus(1, ChronoUnit.DAYS) : DateUtils.stringToDate(fromString));
            Instant to = (toString == null || toString.isEmpty() ? Instant.now() : DateUtils.stringToDate(toString));
            List<Ticket> assetsNotSendingList = rulesDao.getAssetNotSendingTicketsBetween(from, to);
            List<TicketType> returnList = TicketMapper.listToTicketType(assetsNotSendingList);
            return Response.ok(returnList).build();
        } catch (Exception e) {
            LOG.error("[ Error when getting assets not sending between {} and {} ] {} ", fromString, toString, e.getMessage(), e);
            return ErrorHandler.getFault(e);
        }
    }
}
