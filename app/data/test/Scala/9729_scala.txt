package org.sisioh.aws4s.s3

import java.io.{ File, InputStream }
import java.net.URL
import java.util.Date

import com.amazonaws.services.s3.model._
import com.amazonaws.services.s3.{ AmazonS3Client, S3ResponseMetadata }
import com.amazonaws.{ AmazonWebServiceRequest, HttpMethod }
import org.sisioh.aws4s.PimpedType

import scala.collection.JavaConverters._
import scala.util.Try

class RichAmazonS3Client(val underlying: AmazonS3Client) extends AnyVal with PimpedType[AmazonS3Client] {

  def listObjectsAsTry(bucketName: String): Try[ObjectListing] = Try {
    underlying.listObjects(bucketName)
  }

  def listObjectsAsTry(bucketName: String, prefix: String): Try[ObjectListing] = Try {
    underlying.listObjects(bucketName, prefix)
  }

  def listObjectsAsTry(listObjectsRequest: ListObjectsRequest): Try[ObjectListing] = Try {
    underlying.listObjects(listObjectsRequest)
  }

  def listNextBatchOfObjectsAsTry(objectListing: ObjectListing): Try[ObjectListing] = Try {
    underlying.listNextBatchOfObjects(objectListing)
  }

  def listVersionsAsTry(bucketName: String, prefix: String): Try[VersionListing] = Try {
    underlying.listVersions(bucketName, prefix)
  }

  def listNextBatchOfVersionsAsTry(versionListing: VersionListing): Try[VersionListing] = Try {
    underlying.listNextBatchOfVersions(versionListing)
  }

  def listVersionsAsTry(bucketName: String, prefix: String, keyMarker: String, versionIdMarker: String, delimiter: String, maxKeys: Int): Try[VersionListing] = Try {
    underlying.listVersions(bucketName, prefix, keyMarker, versionIdMarker, delimiter, maxKeys)
  }

  def listVersionsAsTry(listVersionsRequest: ListVersionsRequest): Try[VersionListing] = Try {
    underlying.listVersions(listVersionsRequest)
  }

  def getS3AccountOwnerAsTry: Try[Owner] = Try {
    underlying.getS3AccountOwner
  }

  def doesBucketExistAsTry(bucketName: String): Try[Boolean] = Try {
    underlying.doesBucketExist(bucketName)
  }

  def listBucketsAsTry: Try[Seq[Bucket]] = Try {
    underlying.listBuckets().asScala.toVector
  }

  def listBucketsAsTry(listBucketsRequest: ListBucketsRequest): Try[Seq[Bucket]] = Try {
    underlying.listBuckets(listBucketsRequest).asScala.toVector
  }

  def getBucketLocationAsTry(bucketName: String): Try[String] = Try {
    underlying.getBucketLocation(bucketName)
  }

  def getBucketLocationAsTry(getBucketLocationRequest: GetBucketLocationRequest): Try[String] = Try {
    underlying.getBucketLocation(getBucketLocationRequest)
  }

  def createBucketAsTry(createBucketRequest: CreateBucketRequest): Try[Bucket] = Try {
    underlying.createBucket(createBucketRequest)
  }

  def createBucketAsTry(bucketName: String): Try[Bucket] = Try {
    underlying.createBucket(bucketName)
  }

  def createBucketAsTry(bucketName: String, region: Region): Try[Bucket] = Try {
    underlying.createBucket(bucketName, region)
  }

  def createBucketAsTry(bucketName: String, key: String): Try[Bucket] = Try {
    underlying.createBucket(bucketName, key)
  }

  def getObjectAclAsTry(bucketName: String, key: String): Try[AccessControlList] = Try {
    underlying.getObjectAcl(bucketName, key)
  }

  def getObjectAclAsTry(bucketName: String, key: String, versionId: String): Try[AccessControlList] = Try {
    underlying.getObjectAcl(bucketName, key, versionId)
  }

  def setObjectAclAsTry(bucketName: String, key: String, accessControlList: AccessControlList): Try[Unit] = Try {
    underlying.setObjectAcl(bucketName, key, accessControlList)
  }

  def setObjectAclAsTry(bucketName: String, key: String, cannedAccessControlList: CannedAccessControlList): Try[Unit] = Try {
    underlying.setObjectAcl(bucketName, key, cannedAccessControlList)
  }

  def setObjectAclAsTry(bucketName: String, key: String, versionId: String, accessControlList: AccessControlList): Try[Unit] = Try {
    underlying.setObjectAcl(bucketName, key, versionId, accessControlList)
  }

  def setObjectAclAsTry(bucketName: String, key: String, versionId: String, cannedAccessControlList: CannedAccessControlList): Try[Unit] = Try {
    underlying.setObjectAcl(bucketName, key, versionId, cannedAccessControlList)
  }

  def setObjectAclAsTry(setObjectAclRequest: SetObjectAclRequest): Try[Unit] = Try {
    underlying.setObjectAcl(setObjectAclRequest)
  }

  def getBucketAclAsTry(bucketName: String): Try[AccessControlList] = Try {
    underlying.getBucketAcl(bucketName)
  }

  def setBucketAclAsTry(setBucketAclRequest: SetBucketAclRequest): Try[Unit] = Try {
    underlying.setBucketAcl(setBucketAclRequest)
  }

  def getBucketAclAsTry(getBucketAclRequest: GetBucketAclRequest): Try[AccessControlList] = Try {
    underlying.getBucketAcl(getBucketAclRequest)
  }

  def setBucketAclAsTry(bucketName: String, accessControlList: AccessControlList): Try[Unit] = Try {
    underlying.setBucketAcl(bucketName, accessControlList)
  }

  def setBucketAclAsTry(bucketName: String, cannedAccessControlList: CannedAccessControlList): Try[Unit] = Try {
    underlying.setBucketAcl(bucketName, cannedAccessControlList)
  }

  def getObjectMetadataAsTry(bucketName: String, key: String): Try[ObjectMetadata] = Try {
    underlying.getObjectMetadata(bucketName, key)
  }

  def getObjectMetadataAsTry(getObjectMetadataRequest: GetObjectMetadataRequest): Try[ObjectMetadata] = Try {
    underlying.getObjectMetadata(getObjectMetadataRequest)
  }

  def getObjectAsTry(bucket: String, key: String): Try[S3Object] = Try {
    underlying.getObject(bucket, key)
  }

  def getObjectAsTry(getObjectRequest: GetObjectRequest): Try[S3Object] = Try {
    underlying.getObject(getObjectRequest)
  }

  def getObjectAsTry(getObjectRequest: GetObjectRequest, file: File): Try[ObjectMetadata] = Try {
    underlying.getObject(getObjectRequest, file)
  }

  def deleteBucketAsTry(deleteBucketRequest: DeleteBucketRequest): Try[Unit] = Try {
    underlying.deleteBucket(deleteBucketRequest)
  }

  def deleteBucketAsTry(bucketName: String): Try[Unit] = Try {
    underlying.deleteBucket(bucketName)
  }

  def putObjectAsTry(putObjectRequest: PutObjectRequest): Try[PutObjectResult] = Try {
    underlying.putObject(putObjectRequest)
  }

  def putObjectAsTry(bucketName: String, key: String, file: File): Try[PutObjectResult] = Try {
    underlying.putObject(bucketName, key, file)
  }

  def putObjectAsTry(bucketName: String, key: String, inputStream: InputStream, objectMetadata: ObjectMetadata): Try[PutObjectResult] = Try {
    underlying.putObject(bucketName, key, inputStream, objectMetadata)
  }

  def copyObjectAsTry(sourceBucketName: String, sourceKey: String, destinationBucketName: String, destinationKey: String): Try[CopyObjectResult] = Try {
    underlying.copyObject(sourceBucketName, sourceKey, destinationBucketName, destinationKey)
  }

  def copyObjectAsTry(copyObjectRequest: CopyObjectRequest): Try[CopyObjectResult] = Try {
    underlying.copyObject(copyObjectRequest)
  }

  def copyPartAsTry(copyPartRequest: CopyPartRequest): Try[CopyPartResult] = Try {
    underlying.copyPart(copyPartRequest)
  }

  def deleteObjectAsTry(bucketName: String, key: String): Try[Unit] = Try {
    underlying.deleteObject(bucketName, key)
  }

  def deleteObjectAsTry(deleteObjectRequest: DeleteObjectRequest): Try[Unit] = Try {
    underlying.deleteObject(deleteObjectRequest)
  }

  def deleteObjectsAsTry(deleteObjectsRequest: DeleteObjectsRequest): Try[DeleteObjectsResult] = Try {
    underlying.deleteObjects(deleteObjectsRequest)
  }

  def deleteVersionAsTry(bucketName: String, key: String, versionId: String): Try[Unit] = Try {
    underlying.deleteVersion(bucketName, key, versionId)
  }

  def deleteVersionAsTry(deleteVersionRequest: DeleteVersionRequest): Try[Unit] = Try {
    underlying.deleteVersion(deleteVersionRequest)
  }

  def getBucketLoggingConfigurationAsTry(bucketName: String): Try[BucketLoggingConfiguration] = Try {
    underlying.getBucketLoggingConfiguration(bucketName)
  }

  def setBucketLoggingConfigurationAsTry(setBucketLoggingConfigurationRequest: SetBucketLoggingConfigurationRequest): Try[Unit] = Try {
    underlying.setBucketLoggingConfiguration(setBucketLoggingConfigurationRequest)
  }

  def getBucketVersioningConfigurationAsTry(s: String): Try[BucketVersioningConfiguration] = Try {
    underlying.getBucketVersioningConfiguration(s)
  }

  def setBucketVersioningConfigurationAsTry(setBucketVersioningConfigurationRequest: SetBucketVersioningConfigurationRequest): Try[Unit] = Try {
    underlying.setBucketVersioningConfiguration(setBucketVersioningConfigurationRequest)
  }

  def getBucketLifecycleConfigurationAsTry(bucketName: String): Try[BucketLifecycleConfiguration] = Try {
    underlying.getBucketLifecycleConfiguration(bucketName)
  }

  def setBucketLifecycleConfigurationAsTry(bucketName: String, bucketLifecycleConfiguration: BucketLifecycleConfiguration): Try[Unit] = Try {
    underlying.setBucketLifecycleConfiguration(bucketName, bucketLifecycleConfiguration)
  }

  def setBucketLifecycleConfigurationAsTry(setBucketLifecycleConfigurationRequest: SetBucketLifecycleConfigurationRequest): Try[Unit] = Try {
    underlying.setBucketLifecycleConfiguration(setBucketLifecycleConfigurationRequest)
  }

  def deleteBucketLifecycleConfigurationAsTry(s: String): Try[Unit] = Try {
    underlying.deleteBucketLifecycleConfiguration(s)
  }

  def deleteBucketLifecycleConfigurationAsTry(deleteBucketLifecycleConfigurationRequest: DeleteBucketLifecycleConfigurationRequest): Try[Unit] = Try {
    underlying.deleteBucketLifecycleConfiguration(deleteBucketLifecycleConfigurationRequest)
  }

  def getBucketCrossOriginConfigurationAsTry(bucketName: String): Try[BucketCrossOriginConfiguration] = Try {
    underlying.getBucketCrossOriginConfiguration(bucketName)
  }

  def setBucketCrossOriginConfigurationAsTry(s: String, bucketCrossOriginConfiguration: BucketCrossOriginConfiguration): Try[Unit] = Try {
    underlying.setBucketCrossOriginConfiguration(s, bucketCrossOriginConfiguration)
  }

  def setBucketCrossOriginConfigurationAsTry(setBucketCrossOriginConfigurationRequest: SetBucketCrossOriginConfigurationRequest): Try[Unit] = Try {
    underlying.setBucketCrossOriginConfiguration(setBucketCrossOriginConfigurationRequest)
  }

  def deleteBucketCrossOriginConfigurationAsTry(bucketName: String): Try[Unit] = Try {
    underlying.deleteBucketCrossOriginConfiguration(bucketName)
  }

  def deleteBucketCrossOriginConfigurationAsTry(deleteBucketCrossOriginConfigurationRequest: DeleteBucketCrossOriginConfigurationRequest): Try[Unit] = Try {
    underlying.deleteBucketCrossOriginConfiguration(deleteBucketCrossOriginConfigurationRequest)
  }

  def getBucketTaggingConfigurationAsTry(bucketName: String): Try[BucketTaggingConfiguration] = Try {
    underlying.getBucketTaggingConfiguration(bucketName)
  }

  def setBucketTaggingConfigurationAsTry(s: String, bucketTaggingConfiguration: BucketTaggingConfiguration): Try[Unit] = Try {
    underlying.setBucketTaggingConfiguration(s, bucketTaggingConfiguration)
  }

  def setBucketTaggingConfigurationAsTry(setBucketTaggingConfigurationRequest: SetBucketTaggingConfigurationRequest): Try[Unit] = Try {
    underlying.setBucketTaggingConfiguration(setBucketTaggingConfigurationRequest)
  }

  def deleteBucketTaggingConfigurationAsTry(bucketName: String): Try[Unit] = Try {
    underlying.deleteBucketTaggingConfiguration(bucketName)
  }

  def deleteBucketTaggingConfigurationAsTry(deleteBucketTaggingConfigurationRequest: DeleteBucketTaggingConfigurationRequest): Try[Unit] = Try {
    underlying.deleteBucketTaggingConfiguration(deleteBucketTaggingConfigurationRequest)
  }

  def getBucketNotificationConfigurationAsTry(buketName: String): Try[BucketNotificationConfiguration] = Try {
    underlying.getBucketNotificationConfiguration(buketName)
  }

  def setBucketNotificationConfigurationAsTry(setBucketNotificationConfigurationRequest: SetBucketNotificationConfigurationRequest): Try[Unit] = Try {
    underlying.setBucketNotificationConfiguration(setBucketNotificationConfigurationRequest)
  }

  def setBucketNotificationConfigurationAsTry(s: String, bucketNotificationConfiguration: BucketNotificationConfiguration): Try[Unit] = Try {
    underlying.setBucketNotificationConfiguration(s, bucketNotificationConfiguration)
  }

  def getBucketWebsiteConfigurationAsTry(bucketName: String): Try[BucketWebsiteConfiguration] = Try {
    underlying.getBucketWebsiteConfiguration(bucketName)
  }

  def getBucketWebsiteConfigurationAsTry(getBucketWebsiteConfigurationRequest: GetBucketWebsiteConfigurationRequest): Try[BucketWebsiteConfiguration] = Try {
    underlying.getBucketWebsiteConfiguration(getBucketWebsiteConfigurationRequest)
  }

  def setBucketWebsiteConfigurationAsTry(bucketName: String, bucketWebsiteConfiguration: BucketWebsiteConfiguration): Try[Unit] = Try {
    underlying.setBucketWebsiteConfiguration(bucketName, bucketWebsiteConfiguration)
  }

  def setBucketWebsiteConfigurationAsTry(setBucketWebsiteConfigurationRequest: SetBucketWebsiteConfigurationRequest): Try[Unit] = Try {
    underlying.setBucketWebsiteConfiguration(setBucketWebsiteConfigurationRequest)
  }

  def deleteBucketWebsiteConfigurationAsTry(bucketName: String): Try[Unit] = Try {
    underlying.deleteBucketWebsiteConfiguration(bucketName)
  }

  def deleteBucketWebsiteConfigurationAsTry(deleteBucketWebsiteConfigurationRequest: DeleteBucketWebsiteConfigurationRequest): Try[Unit] = Try {
    underlying.deleteBucketWebsiteConfiguration(deleteBucketWebsiteConfigurationRequest)
  }

  def getBucketPolicyAsTry(bucketName: String): Try[BucketPolicy] = Try {
    underlying.getBucketPolicy(bucketName)
  }

  def getBucketPolicyAsTry(getBucketPolicyRequest: GetBucketPolicyRequest): Try[BucketPolicy] = Try {
    underlying.getBucketPolicy(getBucketPolicyRequest)
  }

  def setBucketPolicyAsTry(s: String, s1: String): Try[Unit] = Try {
    underlying.setBucketPolicy(s, s1)
  }

  def setBucketPolicyAsTry(setBucketPolicyRequest: SetBucketPolicyRequest): Try[Unit] = Try {
    underlying.setBucketPolicy(setBucketPolicyRequest)
  }

  def deleteBucketPolicyAsTry(bucketName: String): Try[Unit] = Try {
    underlying.deleteBucketPolicy(bucketName)
  }

  def deleteBucketPolicyAsTry(deleteBucketPolicyRequest: DeleteBucketPolicyRequest): Try[Unit] = Try {
    underlying.deleteBucketPolicy(deleteBucketPolicyRequest)
  }

  def generatePresignedUrlAsTry(bucketName: String, key: String, date: Date): Try[URL] = Try {
    underlying.generatePresignedUrl(bucketName, key, date)
  }

  def generatePresignedUrlAsTry(buketName: String, key: String, date: Date, httpMethod: HttpMethod): Try[URL] = Try {
    underlying.generatePresignedUrl(buketName, key, date, httpMethod)
  }

  def generatePresignedUrlAsTry(generatePresignedUrlRequest: GeneratePresignedUrlRequest): Try[URL] = Try {
    underlying.generatePresignedUrl(generatePresignedUrlRequest)
  }

  def initiateMultipartUploadAsTry(initiateMultipartUploadRequest: InitiateMultipartUploadRequest): Try[InitiateMultipartUploadResult] = Try {
    underlying.initiateMultipartUpload(initiateMultipartUploadRequest)
  }

  def uploadPartAsTry(uploadPartRequest: UploadPartRequest): Try[UploadPartResult] = Try {
    underlying.uploadPart(uploadPartRequest)
  }

  def listPartsAsTry(listPartsRequest: ListPartsRequest): Try[PartListing] = Try {
    underlying.listParts(listPartsRequest)
  }

  def abortMultipartUploadAsTry(abortMultipartUploadRequest: AbortMultipartUploadRequest): Try[Unit] = Try {
    underlying.abortMultipartUpload(abortMultipartUploadRequest)
  }

  def completeMultipartUploadAsTry(completeMultipartUploadRequest: CompleteMultipartUploadRequest): Try[CompleteMultipartUploadResult] = Try {
    underlying.completeMultipartUpload(completeMultipartUploadRequest)
  }

  def listMultipartUploadsAsTry(listMultipartUploadsRequest: ListMultipartUploadsRequest): Try[MultipartUploadListing] = Try {
    underlying.listMultipartUploads(listMultipartUploadsRequest)
  }

  def getCachedResponseMetadataAsTry(amazonWebServiceRequest: AmazonWebServiceRequest): Try[S3ResponseMetadata] = Try {
    underlying.getCachedResponseMetadata(amazonWebServiceRequest)
  }

  def restoreObjectAsTry(restoreObjectRequest: RestoreObjectRequest): Try[Unit] = Try {
    underlying.restoreObject(restoreObjectRequest)
  }

  def restoreObjectAsTry(bucketName: String, key: String, expirationInDays: Int): Try[Unit] = Try {
    underlying.restoreObject(bucketName, key, expirationInDays)
  }

  def enableRequesterPaysAsTry(bucketName: String): Try[Unit] = Try {
    underlying.enableRequesterPays(bucketName)
  }

  def disableRequesterPaysAsTry(bucketName: String): Try[Unit] = Try {
    underlying.disableRequesterPays(bucketName)
  }

  def isRequesterPaysEnabledAsTry(bucketName: String): Try[Boolean] = Try {
    underlying.isRequesterPaysEnabled(bucketName)
  }

  def getResourceUrlAsTry(bucketName: String, key: String) = Try {
    underlying.getResourceUrl(bucketName, key)
  }

  def getUrlAsTry(bucketName: String, key: String) = Try {
    underlying.getUrl(bucketName, key)
  }

  def getRegionAsTry(): Try[com.amazonaws.services.s3.model.Region] = Try {
    underlying.getRegion
  }

}
