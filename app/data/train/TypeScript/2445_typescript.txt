import { Component, OnInit }  from '@angular/core';
import { Page }               from '../objects/page';
import { NavigationService }  from '../services/navigation.service';

@Component({
  selector: 'my-footer',
  moduleId: module.id,
  templateUrl: '../views/footer.component.html'
})

export class FooterComponent implements OnInit
{
  year: string;
  pages: Page[];
  selectedPage: Page;
  constructor(private navigationService: NavigationService) { }

  ngOnInit(): void {
    //get pages
    this.getPages();

    //get year
    this.year = new Date().getFullYear().toString();
  }

  getPages(): void {
    this.navigationService.getPages().then(pages=> this.pages = pages);
  }

  onSelect(page): void {
    this.selectedPage = page;
  }
}
