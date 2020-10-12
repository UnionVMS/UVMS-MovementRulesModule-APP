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
package eu.europa.ec.fisheries.uvms.movementrules.service.entity;

import javax.persistence.*;
import javax.validation.constraints.NotNull;
import javax.xml.bind.annotation.XmlRootElement;
import java.io.Serializable;
import java.time.Instant;
import java.util.UUID;

//@formatter:off
@Entity
@Table(name = "previousreport")
@XmlRootElement
@NamedQueries({
        @NamedQuery(name = PreviousReport.GET_ALL_PREVIOUS_REPORTS, query = "SELECT pr FROM PreviousReport pr"),
        @NamedQuery(name = PreviousReport.FIND_PREVIOUS_REPORT_BY_ASSET_GUID, query = "SELECT pr FROM PreviousReport pr WHERE pr.assetGuid = :assetGuid")
})
//@formatter:on
public class PreviousReport implements Serializable {

    public static final String GET_ALL_PREVIOUS_REPORTS = "PreviousReport.findAll";
    public static final String FIND_PREVIOUS_REPORT_BY_ASSET_GUID = "PreviousReport.findByAssetGuid";
    
    private static final long serialVersionUID = 1L;

    @Id
    @GeneratedValue(strategy = GenerationType.AUTO)
    @Column(name = "prevrep_id")
    private UUID id;

    @Column(name = "prevrep_assetguid", unique = true)
    private String assetGuid;

    @Column(name = "prevrep_mobtermguid")
    private UUID mobTermGuid;

    @Column(name = "prevrep_movementguid")
    private UUID movementGuid;

    @Column(name = "prevrep_positiontime")
    private Instant positionTime;

    @Column(name = "prevrep_updattim")
    @NotNull
    private Instant updated;

    @Column(name = "prevrep_upuser")
    @NotNull
    private String updatedBy;

    public UUID getId() {
        return id;
    }

    public void setId(UUID id) {
        this.id = id;
    }

    public String getAssetGuid() {
        return assetGuid;
    }

    public void setAssetGuid(String assetGuid) {
        this.assetGuid = assetGuid;
    }

    public UUID getMobTermGuid() {
        return mobTermGuid;
    }

    public void setMobTermGuid(UUID mobTermGuid) {
        this.mobTermGuid = mobTermGuid;
    }

    public UUID getMovementGuid() {
        return movementGuid;
    }

    public void setMovementGuid(UUID movementGuid) {
        this.movementGuid = movementGuid;
    }

    public Instant getPositionTime() {
        return positionTime;
    }

    public void setPositionTime(Instant positionTime) {
        this.positionTime = positionTime;
    }

    public Instant getUpdated() {
        return updated;
    }

    public void setUpdated(Instant updated) {
        this.updated = updated;
    }

    public String getUpdatedBy() {
        return updatedBy;
    }

    public void setUpdatedBy(String updatedBy) {
        this.updatedBy = updatedBy;
    }

    @Override
    public String toString() {
        return "PreviousReport{" +
                "id=" + id +
                ", assetGuid='" + assetGuid + '\'' +
                ", positionTime=" + positionTime +
                ", updated=" + updated +
                ", updatedBy='" + updatedBy + '\'' +
                '}';
    }
}