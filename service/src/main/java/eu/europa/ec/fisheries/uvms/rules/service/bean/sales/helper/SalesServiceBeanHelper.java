package eu.europa.ec.fisheries.uvms.rules.service.bean.sales.helper;


import com.google.common.base.Optional;
import eu.europa.ec.fisheries.schema.sales.CheckForUniqueIdResponse;
import eu.europa.ec.fisheries.schema.sales.FLUXSalesReportMessage;
import eu.europa.ec.fisheries.schema.sales.FindReportByIdResponse;
import eu.europa.ec.fisheries.schema.sales.SalesMessageIdType;
import eu.europa.ec.fisheries.uvms.commons.message.api.MessageException;
import eu.europa.ec.fisheries.uvms.rules.message.constants.DataSourceQueue;
import eu.europa.ec.fisheries.uvms.rules.message.consumer.RulesResponseConsumer;
import eu.europa.ec.fisheries.uvms.rules.message.producer.RulesMessageProducer;
import eu.europa.ec.fisheries.uvms.sales.model.exception.SalesMarshallException;
import eu.europa.ec.fisheries.uvms.sales.model.mapper.JAXBMarshaller;
import eu.europa.ec.fisheries.uvms.sales.model.mapper.SalesModuleRequestMapper;
import java.util.List;
import javax.ejb.EJB;
import javax.ejb.Singleton;
import javax.jms.JMSException;
import javax.jms.TextMessage;

import lombok.extern.slf4j.Slf4j;
import org.apache.commons.lang.StringUtils;

@Slf4j
@Singleton
public class SalesServiceBeanHelper {

    @EJB
    private RulesMessageProducer messageProducer;

    @EJB
    private RulesResponseConsumer messageConsumer;

    protected Optional<FLUXSalesReportMessage> receiveMessageFromSales(String correlationId) throws MessageException, JMSException, SalesMarshallException {
        TextMessage receivedMessageAsTextMessage = messageConsumer.getMessage(correlationId, TextMessage.class, 30000L);
        String receivedMessageAsString = receivedMessageAsTextMessage.getText();
        log.info("Received FLUXSalesReportMessage response message from Sales module");
        return unmarshal(receivedMessageAsString);
    }

    protected String sendMessageToSales(String request) throws MessageException {
        return messageProducer.sendDataSourceMessage(request, DataSourceQueue.SALES);
    }


    protected Optional<FLUXSalesReportMessage> unmarshal(String message) throws SalesMarshallException {
        FindReportByIdResponse findReportByIdResponse = JAXBMarshaller.unmarshallString(message, FindReportByIdResponse.class);
        if (StringUtils.isNotBlank(findReportByIdResponse.getReport())) {
            return Optional.of((FLUXSalesReportMessage) JAXBMarshaller.unmarshallString(findReportByIdResponse.getReport(), FLUXSalesReportMessage.class));
        } else {
            return Optional.absent();
        }
    }

    public Optional<FLUXSalesReportMessage> findReport(String guid) throws MessageException, SalesMarshallException, JMSException {
        log.info("Send FLUXSalesReportMessage request message to Sales module");
        String findReportByIdRequest = SalesModuleRequestMapper.createFindReportByIdRequest(guid);
        String correlationId = sendMessageToSales(findReportByIdRequest);
        return receiveMessageFromSales(correlationId);
    }

    public boolean areAnyOfTheseIdsNotUnique(List<String> id, SalesMessageIdType type) throws SalesMarshallException, MessageException, JMSException {
        String checkForUniqueIdRequest = SalesModuleRequestMapper.createCheckForUniqueIdRequest(id, type);
        log.info("Send CheckForUniqueIdRequest request message to Sales module");
        String correlationID = sendMessageToSales(checkForUniqueIdRequest);

        TextMessage receivedMessageAsTextMessage = messageConsumer.getMessage(correlationID, TextMessage.class, 30000L);
        log.info("Received CheckForUniqueIdResponse response message from Sales module");
        CheckForUniqueIdResponse response = JAXBMarshaller.unmarshallString(receivedMessageAsTextMessage.getText(), CheckForUniqueIdResponse.class);
        return !response.isUnique();
    }
}
