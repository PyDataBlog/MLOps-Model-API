import {IQueryParams} from "../components/session/interfaces";


export interface IMediaType {
  key: string;
  label: string;
}


export interface IMediaTypes {
  [i: number]: IMediaType;
}


export interface ISettings {
  API_URL: string;
  DEFAULT_QUERY_PARAMS: IQueryParams;
  ENABLE_DEBUG_LOGGING: Boolean;
  HTTP_CACHE_SIZE: number;
  MEDIA_TYPES: IMediaTypes;
}
