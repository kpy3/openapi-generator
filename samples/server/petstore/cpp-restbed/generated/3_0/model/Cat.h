/**
 * OpenAPI Petstore
 * This spec is mainly for testing Petstore server and contains fake endpoints, models. Please do not use this for any other purpose. Special characters: \" \\
 *
 * The version of the OpenAPI document: 1.0.0
 * 
 *
 * NOTE: This class is auto generated by OpenAPI-Generator 7.13.0-SNAPSHOT.
 * https://openapi-generator.tech
 * Do not edit the class manually.
 */

/*
 * Cat.h
 *
 * 
 */

#ifndef Cat_H_
#define Cat_H_



#include <string>
#include "Animal.h"
#include <memory>
#include <vector>
#include <boost/property_tree/ptree.hpp>
#include "Animal.h"
#include "helpers.h"

namespace org {
namespace openapitools {
namespace server {
namespace model {

/// <summary>
/// 
/// </summary>
class  Cat : public Animal
{
public:
    Cat() = default;
    explicit Cat(boost::property_tree::ptree const& pt);
    virtual ~Cat() = default;

    Cat(const Cat& other) = default; // copy constructor
    Cat(Cat&& other) noexcept = default; // move constructor

    Cat& operator=(const Cat& other) = default; // copy assignment
    Cat& operator=(Cat&& other) noexcept = default; // move assignment

    std::string toJsonString(bool prettyJson = false) const;
    void fromJsonString(std::string const& jsonString);
    boost::property_tree::ptree toPropertyTree() const;
    void fromPropertyTree(boost::property_tree::ptree const& pt);


    /////////////////////////////////////////////
    /// Cat members

    /// <summary>
    /// 
    /// </summary>
    std::string getClassName() const;
    void setClassName(std::string value);

    /// <summary>
    /// 
    /// </summary>
    std::string getColor() const;
    void setColor(std::string value);

    /// <summary>
    /// 
    /// </summary>
    bool isDeclawed() const;
    void setDeclawed(bool value);

protected:
    std::string m_ClassName = "";
    std::string m_Color = "red";
    bool m_Declawed = false;
};

std::vector<Cat> createCatVectorFromJsonString(const std::string& json);

template<>
inline boost::property_tree::ptree toPt<Cat>(const Cat& val) {
    return val.toPropertyTree();
}

template<>
inline Cat fromPt<Cat>(const boost::property_tree::ptree& pt) {
    Cat ret;
    ret.fromPropertyTree(pt);
    return ret;
}

}
}
}
}

#endif /* Cat_H_ */
