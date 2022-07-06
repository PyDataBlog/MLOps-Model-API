import { Component, OnInit } from '@angular/core';
import { ActivatedRoute, Router } from '@angular/router';
//
import { AdminApiService } from '../admin-api.service';
import { Data } from '../data';
//
import { ApiService } from '../../app/api.service';
import { News } from '../../app/news';

@Component({
  selector: 'app-news-edit',
  templateUrl: './news-edit.component.html',
  styleUrls: ['./news-edit.component.css']
})
export class NewsEditComponent implements OnInit {

  news: News;
  type: string;
  editMode: boolean;
  id: any;
  private sub: any;

  constructor(
    private router: Router,
    private apiService: ApiService,
    private adminApiService: AdminApiService,
    private route: ActivatedRoute,
    private data: Data
  ) {

  }

  ngOnInit() {
    //edit or create
    this.sub = this.route.params.subscribe(params => {
      if (params['id']) {
        //edit news
        this.id = params['id'];
        this.editMode = true;
        this.getNews(this.id);
      } else if (params['type']) {
        //create news
        this.news = {
          type: params['type'], subType: '', publisher: this.data.user.name, title: '', content: ''
        }
        console.log(params['type'])
      } else {
        console.log('error');
      }
    });
  }


  getNews(id): void {
    this.apiService
      .getNews(id)
      .subscribe(data => {
        this.news = data;
      });
  }


  updateNews(): void {
    this.news.publisher = this.data.user.name;
    this.adminApiService
      .updateNews(this.id, this.news)
      .subscribe(data => {
        if (data.error_code != 0) {
          alert(data.error_code)
        } else {
          alert('編輯成功');
          this.router.navigate(['/news-list/' + this.news.type]);
        }
      });
  }

  deleteNews(): void {
    this.adminApiService
      .deleteNews(this.id)
      .subscribe(data => {
        if (data.error_code != 0) {
          alert(data.error_code)
        } else {
          alert('刪除成功');
          this.router.navigate(['/news-list/' + this.news.type]);
        }
      });
  }

  createNews(): void {
    this.adminApiService
      .createNews(this.news)
      .subscribe(data => {
        if (data.error_code != 0) {
          alert(data.error_code)
        } else {
          alert('新增成功');
          this.router.navigate(['/news-list/' + this.news.type]);
        }
      })
  }
}