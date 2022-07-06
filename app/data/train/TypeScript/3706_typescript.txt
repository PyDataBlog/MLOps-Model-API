import { CommonModule } from '@angular/common';
import { ToDoItem } from './../shared/models/to-do-item';
import { ToDoService } from './../shared/services/to-do.service';
import { Component, OnInit } from '@angular/core';
import { Status } from "app/shared/models/status.enum";

@Component({
  selector: 'app-home',
  templateUrl: './home.component.html',
  styleUrls: ['./home.component.css']
})
export class HomeComponent implements OnInit {
  message: any;
  currentSelection: string;
  toDoList: Array<ToDoItem>;
  private rawData: Array<ToDoItem>;

  constructor(private toDoService: ToDoService) { }

  ngOnInit() {
    this.rawData = this.toDoService.getItem();
    this.currentSelection = "Current";
    this.filterData(null);
  }

  public submit(): void {
    let toDoItem = new ToDoItem().generate(this.message);
    toDoItem.id = this.toDoService.setItem(toDoItem);
    this.rawData.push(toDoItem);
    this.message = "";
  }
  public updateStatus(id: string, status: Status): void {
    this.toDoService.updateStatus(id, status);
    this.rawData.forEach((item) => {
      if (item.id === id) {
        item.status = status;
      }
    });
  }

  public filterData(statusClicked: Status): void {
    switch (statusClicked) {
      case Status.Due:
        this.currentSelection = "Due";
         this.toDoList = this.rawData.filter((item) => item.status == Status.Due);
        break;
      case Status.Paused:
        this.currentSelection = "Paused";
        this.toDoList = this.rawData.filter((item) => item.status == Status.Paused);
        break;
      default:
        this.currentSelection = "Current";
        this.toDoList = this.rawData.filter((item) => item.status != Status.Completed);
        break;
    }
  }
}
