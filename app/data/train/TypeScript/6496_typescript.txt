import { Injectable } from '@angular/core';
import { BaseHttpService } from "../../shared/services/base-http.service";
import { Http } from "@angular/http";
import { endpoints } from "../../shared/endpoints";

@Injectable()
export class AttachmentApiService extends BaseHttpService {

  constructor(http: Http) {
    super(http);
  }

  upload(formData: FormData) {
    return this.post(endpoints.attachment.create, formData);
  }
}
