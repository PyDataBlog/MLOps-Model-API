import { NgModule } from '@angular/core';
import { Routes, RouterModule } from '@angular/router';

import { BlogComponent } from './blog.component';
import { HeroComponent } from './hero.component';
import { AboutComponent } from './about.component';
import { PricingComponent} from './pricing.component';
import { ContactUsComponent } from './contact-us.component';
import { BlogPostFullComponent } from './blog-post-full.component';

const appRoutes: Routes = [
  { path: '', redirectTo: '/hero', pathMatch: 'full' },
  { path: 'blog', component: BlogComponent },
  { path: 'hero', component: HeroComponent },
  { path: 'about', component: AboutComponent},
  { path: 'pricing', component: PricingComponent},
  { path: 'contact-us', component: ContactUsComponent},
  { path: 'post/:id', component: BlogPostFullComponent }
]

@NgModule({
  imports: [RouterModule.forRoot(appRoutes)],
  exports: [RouterModule]
})

export class AppRoutingModule { }
