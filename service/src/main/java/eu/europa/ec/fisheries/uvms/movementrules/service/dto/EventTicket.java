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
package eu.europa.ec.fisheries.uvms.movementrules.service.dto;

import eu.europa.ec.fisheries.uvms.movementrules.service.entity.CustomRule;
import eu.europa.ec.fisheries.uvms.movementrules.service.entity.Ticket;

public class EventTicket {

    private Ticket ticket;
    private CustomRule customRule;
    
    public EventTicket(Ticket ticket, CustomRule customRule) {
        this.ticket = ticket;
        this.customRule = customRule;
        if (customRule != null) {
            customRule.getRuleSubscriptionList().size();
        }
    }
    
    public Ticket getTicket() {
        return ticket;
    }
    public void setTicket(Ticket ticket) {
        this.ticket = ticket;
    }
    public CustomRule getCustomRule() {
        return customRule;
    }
    public void setCustomRule(CustomRule customRule) {
        this.customRule = customRule;
    }
}
