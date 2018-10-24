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
package eu.europa.ec.fisheries.uvms.movementrules.service.bean;

import com.fasterxml.jackson.databind.DeserializationFeature;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.datatype.jsr310.JavaTimeModule;
import eu.europa.ec.fisheries.schema.exchange.movement.v1.MovementRefTypeType;
import eu.europa.ec.fisheries.schema.movement.v1.MovementType;
import eu.europa.ec.fisheries.schema.movementrules.asset.v1.AssetIdList;
import eu.europa.ec.fisheries.schema.movementrules.asset.v1.AssetIdType;
import eu.europa.ec.fisheries.schema.movementrules.mobileterminal.v1.IdList;
import eu.europa.ec.fisheries.schema.movementrules.mobileterminal.v1.IdType;
import eu.europa.ec.fisheries.schema.movementrules.movement.v1.MovementSourceType;
import eu.europa.ec.fisheries.schema.movementrules.movement.v1.RawMovementType;
import eu.europa.ec.fisheries.uvms.asset.client.model.AssetMTEnrichmentRequest;
import eu.europa.ec.fisheries.uvms.asset.client.model.AssetMTEnrichmentResponse;
import eu.europa.ec.fisheries.uvms.movementrules.service.boundary.ConfigServiceBean;
import eu.europa.ec.fisheries.uvms.movementrules.service.boundary.ExchangeServiceBean;
import eu.europa.ec.fisheries.uvms.movementrules.service.boundary.MovementServiceBean;
import eu.europa.ec.fisheries.uvms.movementrules.service.business.MovementFact;
import eu.europa.ec.fisheries.uvms.movementrules.service.business.RawMovementFact;
import eu.europa.ec.fisheries.uvms.movementrules.service.business.RulesValidator;
import eu.europa.ec.fisheries.uvms.movementrules.service.dao.RulesDao;
import eu.europa.ec.fisheries.uvms.movementrules.service.entity.PreviousReport;
import eu.europa.ec.fisheries.uvms.movementrules.service.exception.RulesServiceException;
import eu.europa.ec.fisheries.uvms.movementrules.service.mapper.MovementFactMapper;
import eu.europa.ec.fisheries.uvms.movementrules.service.mapper.RawMovementFactMapper;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import javax.ejb.Stateless;
import javax.inject.Inject;
import javax.ws.rs.client.Client;
import javax.ws.rs.client.ClientBuilder;
import javax.ws.rs.client.Entity;
import javax.ws.rs.client.WebTarget;
import javax.ws.rs.core.GenericType;
import javax.ws.rs.core.MediaType;
import javax.ws.rs.core.Response;
import javax.ws.rs.ext.ContextResolver;
import java.util.Date;
import java.util.List;
import java.util.UUID;
import java.util.concurrent.*;

@Stateless
public class MovementReportProcessorBean {

    private static final Logger LOG = LoggerFactory.getLogger(MovementReportProcessorBean.class);

    @Inject
    private ExchangeServiceBean exchangeService;

    @Inject
    private MovementServiceBean movementService;

    @Inject
    private ConfigServiceBean configService;

    @Inject
    private RulesValidator rulesValidator;

    @Inject
    private RulesDao rulesDao;

    /*@Inject
    private AssetClient assetClient;*/




    public void setMovementReportReceived(final RawMovementType rawMovement, String pluginType, String username) throws RulesServiceException {
        try {
            Date auditTimestamp = new Date();
            Date auditTotalTimestamp = new Date();

            // the Rest Call

            AssetMTEnrichmentRequest request = createRequest(rawMovement, pluginType,  username);
            //AssetMTEnrichmentResponse response = assetClient.collectAssetMT(request);
            AssetMTEnrichmentResponse response = collectAssetMT(request);


            RawMovementFact rawMovementFact = RawMovementFactMapper.mapRawMovementFact(rawMovement, response, pluginType);
            LOG.debug("rawMovementFact:{}", rawMovementFact);

            //added a loooong series of if statements that does the same thing, check the rulesValidator
            rulesValidator.evaluate(rawMovementFact);
            auditLog("Time to validate sanity:", auditTimestamp);

            if (rawMovementFact.isOk()) {
                MovementFact movementFact = collectMovementData(response, rawMovement, username);
                LOG.info("[INFO] Validating movement from Movement Module");
                rulesValidator.evaluate(movementFact);
                auditLog("Rules total time:", auditTotalTimestamp);
                // Tell Exchange that a movement was persisted in Movement
                exchangeService.sendBackToExchange(movementFact.getMovementGuid(), rawMovement, MovementRefTypeType.MOVEMENT, username);
            } else {
                // Tell Exchange that the report caused an alarm
                exchangeService.sendBackToExchange(null, rawMovement, MovementRefTypeType.ALARM, username);
            }
        } catch (Exception e) {
            LOG.error("setMovementReportReceived failed", e);
            throw new RulesServiceException(e);
        }
    }


    public AssetMTEnrichmentResponse collectAssetMT(AssetMTEnrichmentRequest request) throws Exception {

        Client client = ClientBuilder.newClient();
        client.register(new ContextResolver<ObjectMapper>() {
            @Override
            public ObjectMapper getContext(Class<?> type) {
                ObjectMapper mapper = new ObjectMapper();
                mapper.registerModule(new JavaTimeModule());
                mapper.configure(DeserializationFeature.FAIL_ON_UNKNOWN_PROPERTIES, false);
                return mapper;
            }
        });
        String assetEndpoint = "http://localhost:8080/unionvms/asset/rest/";
        WebTarget webTarget = client.target(assetEndpoint + "internal/");

        // @formatter:off
        Response response =  webTarget
                .path("collectassetmt")
                .request(MediaType.APPLICATION_JSON)
                .accept(MediaType.APPLICATION_JSON)
                .post(Entity.json(request), Response.class);
        // @formatter:on

        AssetMTEnrichmentResponse ret = response.readEntity(new GenericType<AssetMTEnrichmentResponse>() {
        });
        response.close();
        client.close();
        return ret;
    }

    private AssetMTEnrichmentRequest createRequest(RawMovementType rawMovement, String pluginType, String username){

        if(rawMovement == null){
            return null;
        }

        // OBS OBS OBS
        // missing in AssetId
        // GFCM, UVI, ACCAT  = > belg req

        AssetMTEnrichmentRequest req = new AssetMTEnrichmentRequest();

        if(rawMovement.getAssetId() != null){

            List<AssetIdList> assetIdList = rawMovement.getAssetId().getAssetIdList();
            for(AssetIdList assetId : assetIdList){
                String value = assetId.getValue();
                AssetIdType assetIdType = assetId.getIdType();
                switch (assetIdType){
                    case ID :
                    case GUID :
                        UUID wrkUUID = UUID.fromString(value);
                        req.setIdValue(wrkUUID);
                        break;
                    case CFR :
                        req.setCfrValue(value);
                        break;
                    case IRCS :
                        req.setIrcsValue(value);
                        break;
                    case IMO :
                        req.setImoValue(value);
                        break;
                    case MMSI :
                        req.setMmsiValue(value);
                        break;
                }
            }

        }


        if(rawMovement.getMobileTerminal() != null){

            eu.europa.ec.fisheries.schema.movementrules.mobileterminal.v1.MobileTerminalType mobileTerminal = rawMovement.getMobileTerminal();
            List<IdList> mobileTeminalIdList = mobileTerminal.getMobileTerminalIdList();
            for(IdList mobTermId : mobileTeminalIdList){
                String value = mobTermId.getValue();
                IdType idType = mobTermId.getType();
                switch (idType){
                    case SERIAL_NUMBER :
                        req.setSerialNumberValue(value);
                        break;
                    case LES :
                        req.setLesValue(value);
                        break;
                    case DNID :
                        req.setDnidValue(value);
                        break;
                    case MEMBER_NUMBER :
                        req.setMemberNumberValue(value);
                        break;
                }
            }
        }

        if(rawMovement.getSource() != null) {
            req.setTranspondertypeValue(rawMovement.getSource().value());
        }
        if(pluginType != null) {
            req.setPluginType(pluginType);
        }
        if(username != null) {
            req.setUser(username);
        }
        return req;
    }

    private MovementFact collectMovementData(AssetMTEnrichmentResponse response, final RawMovementType rawMovement, final String username) throws ExecutionException, RulesServiceException {
        int threadNum = 5;
        ExecutorService executor = Executors.newFixedThreadPool(threadNum);
        Integer numberOfReportsLast24Hours;
        final String assetGuid;
        final String assetHistGuid;
        final String assetFlagState;
        if (response.getAssetUUID() != null && response.getAssetHistoryId() != null) {
            assetGuid = response.getAssetUUID();
            assetHistGuid = response.getAssetHistoryId();
            assetFlagState = response.getFlagstate();
        } else {
            LOG.warn("[WARN] Asset was null for {} ", rawMovement.getAssetId());
            assetGuid = null;
            assetHistGuid = null;
            assetFlagState = null;
        }

        final Date positionTime = rawMovement.getPositionTime();

        FutureTask<Long> timeDiffAndPersistMovementTask = new FutureTask<>(new Callable<Long>() {
            @Override
            public Long call() {
                return timeDiffAndPersistPreviousReport(rawMovement.getSource(), assetGuid, assetFlagState, positionTime);
            }
        });
        executor.execute(timeDiffAndPersistMovementTask);

        FutureTask<Integer> numberOfReportsLast24HoursTask = new FutureTask<>(new Callable<Integer>() {
            @Override
            public Integer call() {
                return movementService.numberOfReportsLast24Hours(assetHistGuid, positionTime);
            }
        });
        executor.execute(numberOfReportsLast24HoursTask);

        FutureTask<MovementType> sendToMovementTask = new FutureTask<>(new Callable<MovementType>() {
            @Override
            public MovementType call() {
                return movementService.sendToMovement(assetHistGuid, rawMovement, username);
            }
        });
        executor.execute(sendToMovementTask);

        FutureTask<List<String>> vicinityOfTask = new FutureTask<>(new Callable<List<String>>() {
            @Override
            public List<String> call() {
                return movementService.getVicinityOf(rawMovement);
            }
        });
        executor.execute(vicinityOfTask);

        // Get channel guid
        String channelGuid = response.getChannelGuid();

        // Get channel type
        String comChannelType = null;
        if (rawMovement.getComChannelType() != null) {
            comChannelType = rawMovement.getComChannelType().name();
        }

        // Get data from parallel tasks
        try {
            Date auditParallelTimestamp = new Date();
            Long timeDiffInSeconds = timeDiffAndPersistMovementTask.get();
            numberOfReportsLast24Hours = numberOfReportsLast24HoursTask.get();
            MovementType createdMovement = sendToMovementTask.get();
            List<String> vicinityOf = vicinityOfTask.get();
            auditLog("Total time for parallel tasks:", auditParallelTimestamp);

            MovementFact movementFact = MovementFactMapper.mapMovementFact(createdMovement, response, comChannelType,  timeDiffInSeconds, numberOfReportsLast24Hours, channelGuid, vicinityOf);
            LOG.debug("movementFact:{}", movementFact);

            executor.shutdown();
            return movementFact;
        } catch (IllegalArgumentException | NullPointerException | InterruptedException e) {
            executor.shutdown();
            throw new RulesServiceException("Error likely caused by a duplicate movement.", e);
        }
    }

    private Long timeDiffAndPersistPreviousReport(MovementSourceType movementSource, String assetGuid, String assetFlagState, Date positionTime) {
        Date auditTimestamp = new Date();

        // This needs to be done before persisting last report
        Long timeDiffInSeconds = null;
        Long timeDiff = timeDiffFromLastCommunication(assetGuid, positionTime);
        timeDiffInSeconds = timeDiff != null ? timeDiff / 1000 : null;
        auditTimestamp = auditLog("Time to fetch time difference to previous report:", auditTimestamp);

        // We only persist our own last communications that were not from AIS.
        if (configService.isLocalFlagstate(assetFlagState) && !movementSource.equals(MovementSourceType.AIS)) {
            persistLastCommunication(assetGuid, positionTime);
        }
        auditLog("Time to persist the position time:", auditTimestamp);

        return timeDiffInSeconds;
    }

    private Long timeDiffFromLastCommunication(String assetGuid, Date thisTime) {
        LOG.info("[INFO] Fetching time difference to previous movement report");
        Long timeDiff = null;
        try {
            PreviousReport entity = rulesDao.getPreviousReportByAssetGuid(assetGuid);

            Date previousTime = entity.getPositionTime();
            timeDiff = thisTime.getTime() - previousTime.getTime();
        } catch (Exception e) { // there should be a DaoMappingException here but based on this exception and the below comment I am putting it in a comment instead.....
            // If something goes wrong, continue with the other validation
            LOG.error("[ERROR] Error when getting previous report by asset guid {}", e.getMessage());
            LOG.warn("[WARN] Error when fetching time difference of previous movement reports..");
        }
        return timeDiff;
    }

    private void persistLastCommunication(String assetGuid, Date positionTime) {
        PreviousReport entity = rulesDao.getPreviousReportByAssetGuid(assetGuid);
        if (entity == null) {
            entity = new PreviousReport();
        }
        entity.setPositionTime(positionTime);
        entity.setAssetGuid(assetGuid);
        entity.setUpdated(new Date());
        entity.setUpdatedBy("UVMS");
        rulesDao.updatePreviousReport(entity);
    }


    private Date auditLog(String msg, Date lastTimestamp) {
        Date newTimestamp = new Date();
        long duration = newTimestamp.getTime() - lastTimestamp.getTime();
        LOG.debug("[INFO] --> AUDIT - {} {} ms", msg, duration);
        return newTimestamp;
    }
}
