/*
 * Copyright 2018 OpenAPI-Generator Contributors (https://openapi-generator.tech)
 * Copyright 2018 SmartBear Software
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     https://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package org.openapitools.codegen.languages;

import io.swagger.v3.oas.models.OpenAPI;
import io.swagger.v3.oas.models.media.Schema;
import lombok.Setter;
import org.apache.commons.lang3.StringUtils;
import org.openapitools.codegen.*;
import org.openapitools.codegen.meta.FeatureSet;
import org.openapitools.codegen.meta.GeneratorMetadata;
import org.openapitools.codegen.meta.Stability;
import org.openapitools.codegen.meta.features.WireFormatFeature;
import org.openapitools.codegen.model.ModelMap;
import org.openapitools.codegen.model.ModelsMap;
import org.openapitools.codegen.model.OperationsMap;
import org.openapitools.codegen.utils.ModelUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.io.File;
import java.util.*;

import static org.openapitools.codegen.utils.CamelizeOption.LOWERCASE_FIRST_LETTER;
import static org.openapitools.codegen.utils.StringUtils.camelize;

public class JavascriptClosureAngularClientCodegen extends DefaultCodegen implements CodegenConfig {
    private final Logger LOGGER = LoggerFactory.getLogger(JavascriptClosureAngularClientCodegen.class);

    public static final String USE_ES6 = "useEs6";

    @Setter protected boolean useEs6;

    public JavascriptClosureAngularClientCodegen() {
        super();

        generatorMetadata = GeneratorMetadata.newBuilder(generatorMetadata)
                .stability(Stability.BETA)
                .featureSet(
                        FeatureSet.newBuilder(generatorMetadata.getFeatureSet())
                                .excludeWireFormatFeatures(WireFormatFeature.XML).build()
                )
                .build();

        outputFolder = "generated-code/javascript-closure-angular";

        // default HIDE_GENERATION_TIMESTAMP to true
        hideGenerationTimestamp = Boolean.TRUE;

        supportsInheritance = false;
        setReservedWordsLowerCase(Arrays.asList("abstract",
                "continue", "for", "new", "switch", "assert", "default", "if",
                "package", "synchronized", "do", "goto", "private",
                "this", "break", "double", "implements", "protected", "throw",
                "byte", "else", "import", "public", "throws", "case", "enum",
                "instanceof", "return", "transient", "catch", "extends", "int",
                "short", "try", "char", "final", "interface", "static", "void",
                "class", "finally", "const", "super", "while"));

        languageSpecificPrimitives = new HashSet<>(Arrays.asList(
                "string",
                "boolean",
                "number",
                "Object",
                "Blob",
                "Date"));
        instantiationTypes.put("array", "Array");

        typeMapping = new HashMap<>();
        typeMapping.put("Array", "Array");
        typeMapping.put("array", "Array");
        typeMapping.put("List", "Array");
        typeMapping.put("boolean", "boolean");
        typeMapping.put("string", "string");
        typeMapping.put("int", "number");
        typeMapping.put("float", "number");
        typeMapping.put("number", "number");
        typeMapping.put("long", "number");
        typeMapping.put("short", "number");
        typeMapping.put("char", "string");
        typeMapping.put("double", "number");
        typeMapping.put("object", "Object");
        typeMapping.put("Object", "Object");
        typeMapping.put("File", "Blob");
        typeMapping.put("file", "Blob");
        typeMapping.put("integer", "number");
        typeMapping.put("Map", "Object");
        typeMapping.put("map", "Object");
        typeMapping.put("DateTime", "Date");

        importMapping = new HashMap<>();
        defaultIncludes = new HashSet<>(Arrays.asList(
                "Object",
                "Array",
                "Blob"
        ));

        typeMapping.put("binary", "string");

        cliOptions.add(new CliOption(CodegenConstants.HIDE_GENERATION_TIMESTAMP, CodegenConstants.HIDE_GENERATION_TIMESTAMP_DESC)
                .defaultValue(Boolean.TRUE.toString()));
        cliOptions.add(new CliOption(USE_ES6,
                "use ES6 templates")
                .defaultValue(Boolean.FALSE.toString()));
    }

    @Override
    public void processOpts() {
        super.processOpts();

        if (additionalProperties.containsKey(USE_ES6)) {
            setUseEs6(convertPropertyToBooleanAndWriteBack(USE_ES6));
        }
    }

    @Override
    public void preprocessOpenAPI(OpenAPI openAPI) {
        super.preprocessOpenAPI(openAPI);

        if (useEs6) {
            embeddedTemplateDir = templateDir = "Javascript-Closure-Angular/es6";
            apiPackage = "resources";
            apiTemplateFiles.put("api.mustache", ".js");
            supportingFiles.add(new SupportingFile("module.mustache", "", "module.js"));
        } else {
            modelTemplateFiles.put("model.mustache", ".js");
            apiTemplateFiles.put("api.mustache", ".js");
            embeddedTemplateDir = templateDir = "Javascript-Closure-Angular";
            apiPackage = "API.Client";
            modelPackage = "API.Client";
        }
    }

    @Override
    public String getName() {
        return "javascript-closure-angular";
    }

    @Override
    public String getHelp() {
        return "Generates a Javascript AngularJS client library annotated with Google Closure Compiler annotations" +
                "(https://developers.google.com/closure/compiler/docs/js-for-compiler?hl=en)";
    }

    @Override
    public CodegenType getTag() {
        return CodegenType.CLIENT;
    }

    @Override
    public String escapeReservedWord(String name) {
        if (this.reservedWordsMappings().containsKey(name)) {
            return this.reservedWordsMappings().get(name);
        }
        return "_" + name;
    }

    @Override
    public String apiFileFolder() {
        return outputFolder + File.separator + apiPackage().replace('.', File.separatorChar);
    }

    @Override
    public String modelFileFolder() {
        return outputFolder + File.separator + modelPackage().replace('.', File.separatorChar);
    }

    @Override
    public String toVarName(String name) {
        // sanitize name
        name = sanitizeName(name);

        // replace - with _ e.g. created-at => created_at
        name = name.replaceAll("-", "_");

        // if it's all upper case, do nothing
        if (name.matches("^[A-Z_]*$"))
            return name;

        // camelize the variable name
        // pet_id => PetId
        name = camelize(name, LOWERCASE_FIRST_LETTER);

        // for reserved word or word starting with number, append _
        if (isReservedWord(name) || name.matches("^\\d.*"))
            name = escapeReservedWord(name);

        return name;
    }

    @Override
    public String toParamName(String name) {
        // should be the same as variable name
        return toVarName(name);
    }

    @Override
    public String toModelName(String name) {
        if (!StringUtils.isEmpty(modelNamePrefix)) {
            name = modelNamePrefix + "_" + name;
        }

        if (!StringUtils.isEmpty(modelNameSuffix)) {
            name = name + "_" + modelNameSuffix;
        }

        // model name cannot use reserved keyword, e.g. return
        if (isReservedWord(name)) {
            LOGGER.warn("{} (reserved word) cannot be used as model name. Renamed to {}", name, camelize("model_" + name));
            name = "model_" + name; // e.g. return => ModelReturn (after camelize)
        }

        // camelize the model name
        // phone_number => PhoneNumber
        return camelize(name);
    }

    @Override
    public String toModelFilename(String name) {
        // should be the same as the model name
        return toModelName(name);
    }

    @Override
    public String getTypeDeclaration(Schema p) {
        if (ModelUtils.isArraySchema(p)) {
            Schema inner = ModelUtils.getSchemaItems(p);
            return getSchemaType(p) + "<!" + getTypeDeclaration(inner) + ">";
        } else if (ModelUtils.isMapSchema(p)) {
            Schema inner = ModelUtils.getAdditionalProperties(p);
            return "Object<!string, " + getTypeDeclaration(inner) + ">";
        } else if (ModelUtils.isFileSchema(p)) {
            return "Object";
        }
        String type = super.getTypeDeclaration(p);
        if (type.equals("boolean") ||
                type.equals("Date") ||
                type.equals("number") ||
                type.equals("string")) {
            return type;
        }
        return apiPackage + "." + type;
    }

    @Override
    public String getSchemaType(Schema p) {
        String schemaType = super.getSchemaType(p);
        String type = null;
        if (typeMapping.containsKey(schemaType)) {
            type = typeMapping.get(schemaType);
            if (languageSpecificPrimitives.contains(type)) {
                return type;
            }
        } else
            type = schemaType;
        return type;
    }

    @Override
    public ModelsMap postProcessModels(ModelsMap objs) {
        for (ModelMap mo : objs.getModels()) {
            CodegenModel cm = mo.getModel();
            cm.imports = new TreeSet<>(cm.imports);
            for (CodegenProperty var : cm.vars) {
                // handle default value for enum, e.g. available => StatusEnum.available
                if (var.isEnum && var.defaultValue != null && !"null".equals(var.defaultValue)) {
                    var.defaultValue = var.datatypeWithEnum + "." + var.defaultValue;
                }
            }
        }
        return objs;
    }

    @Override
    public OperationsMap postProcessOperationsWithModels(OperationsMap objs, List<ModelMap> allModels) {
        List<Map<String, String>> imports = objs.getImports();
        imports.sort(Comparator.comparing(o -> o.get("import")));
        objs.put("imports", imports);
        return objs;
    }

    @Override
    public String toOperationId(String operationId) {
        // throw exception if method name is empty
        if (StringUtils.isEmpty(operationId)) {
            throw new RuntimeException("Empty method/operation name (operationId) not allowed");
        }

        operationId = camelize(sanitizeName(operationId), LOWERCASE_FIRST_LETTER);

        // method name cannot use reserved keyword, e.g. return
        if (isReservedWord(operationId)) {
            String newOperationId = camelize("call_" + operationId, LOWERCASE_FIRST_LETTER);
            LOGGER.warn("{} (reserved word) cannot be used as method name. Renamed to {}", operationId, newOperationId);
            return newOperationId;
        }

        return operationId;
    }

    @Override
    public String escapeQuotationMark(String input) {
        // remove ', " to avoid code injection
        return input.replace("\"", "").replace("'", "");
    }

    @Override
    public String escapeUnsafeCharacters(String input) {
        return input.replace("*/", "*_/").replace("/*", "/_*");
    }

    @Override
    public GeneratorLanguage generatorLanguage() {
        return GeneratorLanguage.JAVASCRIPT;
    }
}
