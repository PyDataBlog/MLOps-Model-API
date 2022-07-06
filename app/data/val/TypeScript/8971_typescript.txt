import {Component, OnInit, Input} from '@angular/core';
import {MetricsStatistics} from "../../shared/models/detailed-statistics.metadata";

@Component({
  selector: 'app-metrics-stats',
  templateUrl: './metrics-stats.component.html',
  styleUrls: ['metrics-stats.component.scss']
})
export class MetricsStatsComponent implements OnInit {
  @Input() metricsOverallState: boolean; // success - fail
  @Input() metricsStats: MetricsStatistics;

  constructor() { }

  ngOnInit() {
  }

}
