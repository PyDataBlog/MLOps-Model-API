import {Component, OnInit} from '@angular/core'

@Component({
  selector: 'app-tool-bar',
  templateUrl: './tool-bar.component.html',
  styleUrls: ['./tool-bar.component.scss']
})
export class ToolBarComponent implements OnInit {

  mockLines: [
    {
      name: 'S1'
    },
    {
      name: 'S2'
    },
    {
      name: 'S3'
    }
  ]
  selectedLine: any

  constructor() { }

  ngOnInit() {
  }

}
