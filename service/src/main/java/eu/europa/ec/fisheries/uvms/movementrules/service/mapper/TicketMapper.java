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
package eu.europa.ec.fisheries.uvms.movementrules.service.mapper;

import java.time.Instant;
import java.util.ArrayList;
import java.util.List;
import eu.europa.ec.fisheries.schema.movementrules.ticket.v1.TicketStatusType;
import eu.europa.ec.fisheries.schema.movementrules.ticket.v1.TicketType;
import eu.europa.ec.fisheries.uvms.movementrules.service.business.MRDateUtils;
import eu.europa.ec.fisheries.uvms.movementrules.service.entity.Ticket;

public class TicketMapper {

    private TicketMapper() {}
    
    public static TicketType toTicketType(TicketType ticketType, Ticket ticketEntity) {
        if (ticketEntity == null) {
            return null;
        }
        ticketType.setAssetGuid(ticketEntity.getAssetGuid());
        ticketType.setMobileTerminalGuid(ticketEntity.getMobileTerminalGuid());
        ticketType.setChannelGuid(ticketEntity.getChannelGuid());
        ticketType.setGuid(ticketEntity.getGuid());
        ticketType.setStatus(TicketStatusType.valueOf(ticketEntity.getStatus()));
        ticketType.setOpenDate(MRDateUtils.dateToString(ticketEntity.getCreatedDate()));
        ticketType.setUpdated(MRDateUtils.dateToString(ticketEntity.getUpdated()));
        ticketType.setUpdatedBy(ticketEntity.getUpdatedBy());
        ticketType.setRuleGuid(ticketEntity.getRuleGuid());
        ticketType.setMovementGuid(ticketEntity.getMovementGuid());
        ticketType.setRuleName(ticketEntity.getRuleName());
        ticketType.setRecipient(ticketEntity.getRecipient());
        if (ticketEntity.getTicketCount() != null) {
            ticketType.setTicketCount(ticketEntity.getTicketCount());
        }

        return ticketType;
    }

    public static Ticket toTicketEntity(Ticket ticketEntity, TicketType ticketType) {
        ticketEntity.setAssetGuid(ticketType.getAssetGuid());
        ticketEntity.setMobileTerminalGuid(ticketType.getMobileTerminalGuid());
        ticketEntity.setChannelGuid(ticketType.getChannelGuid());
        ticketEntity.setGuid(ticketType.getGuid());
        ticketEntity.setStatus(ticketType.getStatus().name());
        ticketEntity.setCreatedDate(MRDateUtils.stringToDate(ticketType.getOpenDate()));
        ticketEntity.setRuleGuid(ticketType.getRuleGuid());
        ticketEntity.setUpdated(Instant.now());
        ticketEntity.setUpdatedBy(ticketType.getUpdatedBy());
        ticketEntity.setMovementGuid(ticketType.getMovementGuid());
        ticketEntity.setRuleName(ticketType.getRuleName());
        ticketEntity.setRecipient(ticketType.getRecipient());

        return ticketEntity;
    }

    public static Ticket toTicketEntity(TicketType ticketType) {
        Ticket ticketEntity = new Ticket();
        return toTicketEntity(ticketEntity, ticketType);
    }

    public static TicketType toTicketType(Ticket ticketEntity) {
        TicketType ticketType = new TicketType();
        return toTicketType(ticketType, ticketEntity);
    }

    public static List<TicketType> listToTicketType(List<Ticket> ticketList) {
        List<TicketType> response = new ArrayList<>();
        for (Ticket ticket : ticketList) {
            response.add(toTicketType(ticket));
        }
        return response;
    }
}