import { Component, HostListener } from '@angular/core';

import { RedditService } from '../../shared/services/reddit.service';

import { Comment } from '../../shared/models/comment';

@Component({
	selector: 'my-app',
	templateUrl: 'app/components/main/app.component.html',
	styleUrls: ['app/components/main/app.css'],
	providers: [RedditService]
})

export class AppComponent {
	constructor(private redditService: RedditService) { }

	name: string = 'Steven';
	content: string = 'fuckits';
	currentImage: string;
	comments: Comment[];
	windowObject:any;
	imageHeight:string;
	currentPostPosition:number = 0;
	arrayOfPostUrls: Array<string> = [];

	@HostListener('window:keydown', ['$event'])
	onKeyDown(event) {
		if (event.code==='ArrowRight'){
			if (this.currentPostPosition<this.arrayOfPostUrls.length){
				this.currentPostPosition+=1;
				this.getRealGirlsPosts();
			}
		}
		else if (event.code==='ArrowLeft'){
			if (this.currentPostPosition!=0){
				this.currentPostPosition-=1;
				this.getRealGirlsPosts();
			}
		}
	}

	ngOnInit(){
		console.log('I am inniting');
		this.getRealGirlsPosts();
		// size the image
		this.windowObject = window["mainScreen"]; //access global (renderer.js is not working so i moved the global to index.html)
		console.log(this.windowObject.workAreaSize.height);
		this.imageHeight = String(Number(this.windowObject.workAreaSize.height)-100);
	}

	getComments(permalink) {
		this.redditService.getDetailedRedditPost(permalink).subscribe(data => {
			console.log(data);

			this.currentImage = data[0].data.children[0].data.url;

			this.comments = data[1].data.children;

			console.log(this.comments);
		});
	}

	getRealGirlsPosts(){
		this.redditService.getRealGirlsPosts()
			.subscribe(data=>{
				console.log('real girls');
				console.log(data);
				data.data.children.forEach((post, key)=>{
					console.log(post.data.permalink);
					this.arrayOfPostUrls.push(post.data.permalink);
				});

				this.getComments(this.arrayOfPostUrls[this.currentPostPosition]);
			})
	}

	setName(newName: string) {
		this.name = newName;
	}
}
