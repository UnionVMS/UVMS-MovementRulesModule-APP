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
package eu.europa.ec.fisheries.uvms.movementrules.service.message.producer;

import javax.ejb.Local;
import javax.jms.TextMessage;

import eu.europa.ec.fisheries.schema.movementrules.common.v1.RulesFault;
import eu.europa.ec.fisheries.uvms.commons.message.api.MessageException;
import eu.europa.ec.fisheries.uvms.movementrules.service.message.constants.DataSourceQueue;

@Local
public interface RulesMessageProducer {

    String sendDataSourceMessage(String text, DataSourceQueue queue, String function, String grouping) throws MessageException;

    String sendResponseMessageForTest(String text, String requestType);

    void sendModuleResponseMessage(TextMessage message, String text) throws MessageException;

    void sendModuleErrorResponseMessage(RulesFault fault, TextMessage message);
}