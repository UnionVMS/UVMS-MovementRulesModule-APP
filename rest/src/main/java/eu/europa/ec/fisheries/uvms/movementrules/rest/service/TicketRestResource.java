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
import eu.europa.ec.fisheries.uvms.movementrules.rest.dto.ResponseCode;
import eu.europa.ec.fisheries.uvms.movementrules.rest.dto.ResponseDto;
import eu.europa.ec.fisheries.uvms.movementrules.rest.error.ErrorHandler;
import eu.europa.ec.fisheries.uvms.movementrules.service.bean.RulesServiceBean;
import eu.europa.ec.fisheries.uvms.movementrules.service.bean.ValidationServiceBean;
import eu.europa.ec.fisheries.uvms.movementrules.service.dto.TicketListResponseDto;
import eu.europa.ec.fisheries.uvms.movementrules.service.entity.Ticket;
import eu.europa.ec.fisheries.uvms.movementrules.service.mapper.TicketMapper;
import eu.europa.ec.fisheries.uvms.rest.security.RequiresFeature;
import eu.europa.ec.fisheries.uvms.rest.security.UnionVMSFeature;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import javax.ejb.EJB;
import javax.ejb.Stateless;
import javax.ws.rs.*;
import javax.ws.rs.core.MediaType;
import java.util.List;
import java.util.UUID;

@Path("/tickets")
@Stateless
@Consumes(value = { MediaType.APPLICATION_JSON })
@Produces(value = { MediaType.APPLICATION_JSON })
public class TicketRestResource {

    private static final Logger LOG = LoggerFactory.getLogger(TicketRestResource.class);

    @EJB
    private RulesServiceBean rulesService;

    @EJB
    private ValidationServiceBean validationService;

    @POST
    @Path("/list/{loggedInUser}")
    @RequiresFeature(UnionVMSFeature.viewAlarmsOpenTickets)
    public ResponseDto getTicketListByQuery(@PathParam("loggedInUser") String loggedInUser, TicketQuery query) {
        LOG.info("Get tickets list invoked in rest layer");
        try {
            TicketListResponseDto ticketList = rulesService.getTicketList(loggedInUser, query);
            GetTicketListByQueryResponse response = new GetTicketListByQueryResponse();
            response.setCurrentPage(ticketList.getCurrentPage());
            response.setTotalNumberOfPages(ticketList.getTotalNumberOfPages());
            response.getTickets().addAll(TicketMapper.listToTicketType(ticketList.getTicketList()));
            return new ResponseDto<>(response, ResponseCode.OK);
        } catch (Exception ex) {
            LOG.error("[ERROR] Error when getting ticket list by query ] {} ", ex.getMessage());
            return ErrorHandler.getFault(ex);
        }
    }

    @POST
    @Path("/listByMovements")
    @RequiresFeature(UnionVMSFeature.viewAlarmsOpenTickets)
    public ResponseDto getTicketsByMovementGUIDs(List<String> movements) {
        LOG.info("Get tickets by movements invoked in rest layer");
        try {
            List<Ticket> tickets = rulesService.getTicketsByMovements(movements);
            GetTicketListByMovementsResponse response = new GetTicketListByMovementsResponse();
            response.getTickets().addAll(TicketMapper.listToTicketType(tickets));
            return new ResponseDto<>(response, ResponseCode.OK);
        } catch (Exception ex) {
            LOG.error("[ Error when getting ticket list by movements. ] {} ", ex);
            return ErrorHandler.getFault(ex);
        }
    }

    @POST
    @Path("/countByMovements")
    @RequiresFeature(UnionVMSFeature.viewAlarmsOpenTickets)
    public ResponseDto countTicketsByMovementGUIDs(List<String> movements) {
        try {
            return new ResponseDto<>(rulesService.countTicketsByMovements(movements), ResponseCode.OK);
        } catch (Exception e) {
            LOG.error("[ Error when counting number of open tickets by movements. ] {} ", e);
            return ErrorHandler.getFault(e);
        }
    }

    @PUT
    @Path("/status")
    @RequiresFeature(UnionVMSFeature.manageAlarmsOpenTickets)
    public ResponseDto updateTicketStatus(final TicketType ticketType) {
        LOG.info("Update ticket status invoked in rest layer");
        try {
            TicketType response = TicketMapper.toTicketType(rulesService.updateTicketStatus(TicketMapper.toTicketEntity(ticketType)));
            return new ResponseDto<>(response, ResponseCode.OK);
        } catch (Exception e) {
            LOG.error("[ Error when updating ticket. ] {} ", e);
            return ErrorHandler.getFault(e);
        }
    }

    @POST
    @Path("/status/{loggedInUser}/{status}")
    @RequiresFeature(UnionVMSFeature.manageAlarmsOpenTickets)
    public ResponseDto updateTicketStatusByQuery(@PathParam("loggedInUser") String loggedInUser, TicketQuery query,
                                                 @PathParam("status") TicketStatusType status) {
        LOG.info("Update ticket status invoked in rest layer");
        try {
            List response = TicketMapper.listToTicketType(rulesService.updateTicketStatusByQuery(loggedInUser, query, status));
            return new ResponseDto<>(response, ResponseCode.OK);
        } catch (Exception e) {
            LOG.error("[ Error when updating tickets. ] {} ", e);
            return ErrorHandler.getFault(e);
        }
    }

    @GET
    @Path("/{guid}")
    @RequiresFeature(UnionVMSFeature.viewAlarmsOpenTickets)
    public ResponseDto getTicketByGuid(@PathParam("guid") String guid) {
        try {
            TicketType response = TicketMapper.toTicketType(rulesService.getTicketByGuid(UUID.fromString(guid)));
            return new ResponseDto<>(response, ResponseCode.OK);
        } catch (Exception e) {
            LOG.error("[ Error when getting ticket by GUID. ] {} ", e);
            return ErrorHandler.getFault(e);
        }
    }

    @GET
    @Path("/countopen/{loggedInUser}")
    @RequiresFeature(UnionVMSFeature.viewAlarmsOpenTickets)
    public ResponseDto getNumberOfOpenTicketReports(@PathParam(value = "loggedInUser") final String loggedInUser) {
        try {
            return new ResponseDto<>(validationService.getNumberOfOpenTickets(loggedInUser), ResponseCode.OK);
        } catch (Exception e) {
            LOG.error("[ Error when getting number of open tickets. ] {} ", e);
            return ErrorHandler.getFault(e);
        }
    }

    /**
     *
     * @responseMessage 200 Number of open tickets for logged in user
     * @responseMessage 500 Error
     *
     * @summary Get number of not sending transponders (used by dashboard widget)
     *
     */
    @GET
    @Path("/countAssetsNotSending")
    @RequiresFeature(UnionVMSFeature.viewAlarmsOpenTickets)
    public ResponseDto getNumberOfAssetsNotSending() {
        try {
            return new ResponseDto<>(rulesService.getNumberOfAssetsNotSending(), ResponseCode.OK);
        } catch (Exception e) {
            LOG.error("[ Error when getting number of assets not sending. ] {} ", e);
            return ErrorHandler.getFault(e);
        }
    }
}
