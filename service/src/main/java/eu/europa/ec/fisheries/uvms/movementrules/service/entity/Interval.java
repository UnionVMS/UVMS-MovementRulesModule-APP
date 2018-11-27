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

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.FetchType;
import javax.persistence.GeneratedValue;
import javax.persistence.GenerationType;
import javax.persistence.Id;
import javax.persistence.JoinColumn;
import javax.persistence.ManyToOne;
import javax.persistence.Table;
import javax.xml.bind.annotation.XmlRootElement;
import java.io.Serializable;
import java.time.Instant;
import java.util.Objects;

//@formatter:off
@Entity
@Table(name = "interval")
@XmlRootElement
//@formatter:on
public class Interval implements Serializable {

    private static final long serialVersionUID = 1L;

    @Id
    @GeneratedValue(strategy = GenerationType.AUTO)
    @Column(name = "interval_id")
    private Long id;

    @Column(name = "interval_start")
    private Instant start;

    @Column(name = "interval_end")
    private Instant end;

    @JoinColumn(name = "interval_rule_id", referencedColumnName = "rule_id")
    @ManyToOne(fetch = FetchType.LAZY)
    private CustomRule customRule;

    public Long getId() {
        return id;
    }

    public void setId(Long id) {
        this.id = id;
    }

    public Instant getStart() {
        return start;
    }

    public void setStart(Instant start) {
        this.start = start;
    }

    public Instant getEnd() {
        return end;
    }

    public void setEnd(Instant end) {
        this.end = end;
    }

    public CustomRule getCustomRule() {
        return customRule;
    }

    public void setCustomRule(CustomRule customRule) {
        this.customRule = customRule;
    }

    @Override
    public int hashCode() {

        return Objects.hash(id, start, end, customRule);
    }

    @Override
    public boolean equals(Object obj) {
        if (obj instanceof Interval) {

            Interval other = (Interval) obj;
            if (start != null && !start.equals(other.start)) {
                return false;
            } else if (start == null && other.start != null) {
                return false;
            }
            if (end != null && !end.equals(other.end)) {
                return false;
            } else if (end == null && other.end != null) {
                return false;
            }

            return true;
        }
        return false;
    }
}