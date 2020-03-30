package com.acrylplatform.api.http.swagger

import akka.actor.ActorSystem
import akka.stream.ActorMaterializer
import com.acrylplatform.Version
import com.acrylplatform.settings.RestAPISettings
import com.github.swagger.akka.SwaggerHttpService
import com.github.swagger.akka.model.{Info, License}
import io.swagger.models.auth.{ApiKeyAuthDefinition, In}
import io.swagger.models.{Scheme, SecurityRequirement, Swagger}

class SwaggerDocService(val actorSystem: ActorSystem, val materializer: ActorMaterializer, val apiClasses: Set[Class[_]], settings: RestAPISettings)
    extends SwaggerHttpService {

  override val host: String = settings.bindAddress + ":" + settings.port
  override val info: Info = Info(
    "The Web Interface to the Acryl Full Node API",
    Version.VersionString,
    "Acryl Full Node",
    "License: MIT License",
    None,
    Some(License("MIT License", "https://github.com/acrylplatform/Acryl/blob/master/LICENSE"))
  )

  //Let swagger-ui determine the host and port
  override val swaggerConfig: Swagger = new Swagger()
    .basePath(SwaggerHttpService.prependSlashIfNecessary(basePath))
    .info(info)
    .scheme(Scheme.HTTPS)
    .scheme(Scheme.HTTP)
    .security(new SecurityRequirement().requirement(SwaggerDocService.apiKeyDefinitionName))
    .securityDefinition(SwaggerDocService.apiKeyDefinitionName, new ApiKeyAuthDefinition("X-API-Key", In.HEADER))
}

object SwaggerDocService {
  final val apiKeyDefinitionName = "API Key"
}
