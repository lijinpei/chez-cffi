#include "clang/AST/ASTConsumer.h"
#include "clang/AST/Decl.h"
#include "clang/AST/DeclGroup.h"
#include "clang/AST/DeclarationName.h"
#include "clang/AST/Type.h"
#include "clang/Frontend/CompilerInstance.h"
#include "clang/Frontend/FrontendAction.h"
#include "clang/Frontend/FrontendPluginRegistry.h"
#include "llvm/ADT/DenseMap.h"
#include "llvm/ADT/StringRef.h"
#include "llvm/Support/Registry.h"

#include "fmt/format.h"
#include "fmt/ostream.h"
#include "scheme.hpp"

#include <fstream>
#include <iostream>
#include <memory>
#include <ostream>
#include <string>
#include <unordered_set>
#include <utility>
#include <vector>

namespace cs = chezscheme;

namespace {

template <std::size_t N>
inline bool try_replace_string(const char (&arr)[N], std::string& name) {
  if (N == 0) {
    return false;
  }
  if (name.size() < N) {
    return false;
  }
  constexpr std::size_t CN = N - 1;
  for (std::size_t i = 0; i < CN; ++i) {
    if (name[i] != arr[i]) {
      return false;
    }
  }
  if (name[CN] == ' ') {
    name[CN] = '-';
    return true;
  }
  return false;
}

std::string santize_name(std::string&& name) {
  constexpr char n1[] = "struct";
  constexpr char n2[] = "union";
  constexpr char n3[] = "enum";
  try_replace_string(n1, name) || try_replace_string(n2, name) ||
      try_replace_string(n3, name);
  return name;
}

cs::ptr cs_array_symbol, cs_struct_symbol, cs_union_symbol, cs_void_symbol,
    cs_bool_symbol, cs_char_symbol, cs_unsigned8_symbol, cs_wchar_symbol,
    cs_unsigned16_symbol, cs_unsigned32_symbol, cs_unsigned_short_symbol,
    cs_unsigned_int_symbol, cs_unsigned_long_symbol,
    cs_unsigned_long_long_symbol, cs_integer8_symbol, cs_short_symbol,
    cs_int_symbol, cs_long_symbol, cs_long_long_symbol, cs_float_symbol,
    cs_double_symbol, cs_function_symbol, cs_pointerstar_symbol,
    cs_define_constant_symbol;
cs::ptr cs_display, cs_newline;
bool scheme_initialized = false;

bool Init(std::vector<std::string> boot_files) {
  if (scheme_initialized) {
    llvm::errs() << "chez scheme already initialized\n";
    return false;
  }
  cs::Sscheme_init(nullptr);
  for (const auto& bf : boot_files) {
    cs::Sregister_boot_file(bf.data());
  }
  cs::Sbuild_heap(nullptr, nullptr);

  cs_array_symbol = cs::Sstring_to_symbol("array");
  cs_struct_symbol = cs::Sstring_to_symbol("struct");
  cs_union_symbol = cs::Sstring_to_symbol("union");
  cs_void_symbol = cs::Sstring_to_symbol("void");
  cs_bool_symbol = cs::Sstring_to_symbol("bool");
  cs_char_symbol = cs::Sstring_to_symbol("char");
  cs_unsigned8_symbol = cs::Sstring_to_symbol("unsigned-8");
  cs_wchar_symbol = cs::Sstring_to_symbol("wchar_t");
  cs_unsigned16_symbol = cs::Sstring_to_symbol("unsigned-16");
  cs_unsigned32_symbol = cs::Sstring_to_symbol("unsigned-32");
  cs_unsigned_short_symbol = cs::Sstring_to_symbol("unsigned-short");
  cs_unsigned_int_symbol = cs::Sstring_to_symbol("unsigned-int");
  cs_unsigned_long_symbol = cs::Sstring_to_symbol("unsigned-long");
  cs_unsigned_long_long_symbol = cs::Sstring_to_symbol("unsigned-long-long");
  cs_integer8_symbol = cs::Sstring_to_symbol("interger-8");
  cs_short_symbol = cs::Sstring_to_symbol("short");
  cs_int_symbol = cs::Sstring_to_symbol("int");
  cs_long_symbol = cs::Sstring_to_symbol("long");
  cs_long_long_symbol = cs::Sstring_to_symbol("long-long");
  cs_float_symbol = cs::Sstring_to_symbol("float");
  cs_double_symbol = cs::Sstring_to_symbol("double");
  cs_function_symbol = cs::Sstring_to_symbol("function");
  cs_pointerstar_symbol = cs::Sstring_to_symbol("*");
  cs_define_constant_symbol = cs::Sstring_to_symbol("define-constant");

  cs_display = cs::Stop_level_value(cs::Sstring_to_symbol("display"));
  cs_newline = cs::Stop_level_value(cs::Sstring_to_symbol("newline"));

  scheme_initialized = true;
  return true;
}

bool Deinit() {
  if (!scheme_initialized) {
    llvm::errs() << "chez scheme not initialized\n";
    return false;
  }
  cs::Sscheme_deinit();
  scheme_initialized = false;
}

bool TryInit(std::vector<std::string> boot_files) {
  return scheme_initialized || Init(std::move(boot_files));
}

bool TryDeinit() { return !scheme_initialized || Deinit(); }

struct DeinitGuard {
  ~DeinitGuard() { TryDeinit(); }
};

DeinitGuard dg;

cs::ptr scheme_list(cs::ptr arg) { return cs::Scons(arg, cs::snil); }

template <typename... T>
cs::ptr scheme_list(cs::ptr arg0, T... arg) {
  return cs::Scons(arg0, scheme_list(arg...));
}

template <bool name_ok>
cs::ptr make_ftype(clang::QualType qt);

bool trace_clang = false;
template <bool name_ok>
cs::ptr make_ftype(const clang::Type* type) {
  if (auto* et = llvm::dyn_cast<clang::ElaboratedType>(type)) {
    //return make_ftype<name_ok>(et->getNamedType());
    if (trace_clang) {
      llvm::errs() << "ElaboratedType\n";
    }
    return make_ftype<name_ok>(et->desugar());
    et->desugar();
    et->getNamedType();
    return cs::sfalse;
  }
  if (auto* at = llvm::dyn_cast<clang::ArrayType>(type)) {
    if (trace_clang) {
      llvm::errs() << "ArrayType\n";
    }
    if (auto* ca = llvm::dyn_cast<clang::ConstantArrayType>(at)) {
      cs::ptr et = make_ftype<true>(at->getElementType());
      cs::ptr num = chezscheme::sfixnum(ca->getSize().getLimitedValue());
      return scheme_list(cs_array_symbol, num, et);
    } else {
      llvm::errs() << "unable to handle non fixed-size array\n";
      type->dump();
      return cs::sfalse;
    }
  }
  if (auto* at = llvm::dyn_cast<clang::AtomicType>(type)) {
    if (trace_clang) {
      llvm::errs() << "AtomicType\n";
    }
    return make_ftype<name_ok>(at->getValueType());
  }
  if (auto* bt = llvm::dyn_cast<clang::BuiltinType>(type)) {
    if (trace_clang) {
      llvm::errs() << "BuiltinType\n";
    }
    if (bt->isSugared()) {
      return make_ftype<name_ok>(bt->desugar());
    }
    switch (bt->getKind()) {
      case clang::BuiltinType::Void:
        return cs_void_symbol;
      case clang::BuiltinType::Bool:
        return cs_bool_symbol;
      case clang::BuiltinType::Char_U:
        return cs_char_symbol;
      case clang::BuiltinType::UChar:
        return cs_unsigned8_symbol;
      case clang::BuiltinType::WChar_U:
        return cs_wchar_symbol;
      case clang::BuiltinType::Char8:
        return cs_unsigned8_symbol;
      case clang::BuiltinType::Char16:
        return cs_unsigned16_symbol;
      case clang::BuiltinType::Char32:
        return cs_unsigned32_symbol;
      case clang::BuiltinType::UShort:
        return cs_unsigned_short_symbol;
      case clang::BuiltinType::UInt:
        return cs_unsigned_int_symbol;
      case clang::BuiltinType::ULong:
        return cs_unsigned_long_symbol;
      case clang::BuiltinType::ULongLong:
        return cs_unsigned_long_long_symbol;
      case clang::BuiltinType::Char_S:
        return cs_char_symbol;
      case clang::BuiltinType::SChar:
        return cs_integer8_symbol;
      case clang::BuiltinType::WChar_S:
        return cs_wchar_symbol;
      case clang::BuiltinType::Short:
        return cs_short_symbol;
      case clang::BuiltinType::Int:
        return cs_int_symbol;
      case clang::BuiltinType::Long:
        return cs_long_symbol;
      case clang::BuiltinType::LongLong:
        return cs_long_long_symbol;
      case clang::BuiltinType::Float:
        return cs_float_symbol;
      case clang::BuiltinType::Double:
        return cs_double_symbol;
      default:
        break;
    }
    llvm::errs() << "unknown builtin type in make_ftype:";
    type->dump();
    return cs::sfalse;
  }
  if (auto* ft = llvm::dyn_cast<clang::FunctionType>(type)) {
    if (trace_clang) {
      llvm::errs() << "FunctionType\n";
    }
    cs::ptr ret = make_ftype<true>(ft->getReturnType());
    cs::ptr par = cs::snil;
    if (llvm::dyn_cast<clang::FunctionNoProtoType>(ft)) {
      return scheme_list(cs_function_symbol, par, ret);
    }
    if (auto* fp = llvm::dyn_cast<clang::FunctionProtoType>(ft)) {
      auto params = fp->getParamTypes();
      for (size_t s = params.size(); s > 0;) {
        par = cs::Scons(make_ftype<true>(params[--s]), par);
      }
      return scheme_list(cs_function_symbol, par, ret);
    }
    llvm::errs() << "meet function-type which is neither FunctionProtoType "
                    "Nor FunctionNoProtoType\n";
    type->dump();
    return cs::sfalse;
  }
  if (auto* pt = llvm::dyn_cast<clang::ParenType>(type)) {
    if (trace_clang) {
      llvm::errs() << "ParenType\n";
    }
    return make_ftype<name_ok>(pt->desugar());
  }
  if (auto* pt = llvm::dyn_cast<clang::PointerType>(type)) {
    if (trace_clang) {
      llvm::errs() << "PointerType\n";
    }
    return scheme_list(cs_pointerstar_symbol,
                       make_ftype<true>(pt->getPointeeType()));
  }
  if (auto* et = llvm::dyn_cast<clang::EnumType>(type)) {
    llvm::StringRef name = et->getDecl()->getName();
    std::string nss = "enum-";
    nss += std::string(name.data(), name.size());
    return cs::Sstring_to_symbol(nss.data());
  }
  if (auto* tt = llvm::dyn_cast<clang::TagType>(type)) {
    if (trace_clang) {
      llvm::errs() << "TagType: " << '\n';
    }
    if (name_ok) {
      if (const char* name = tt->getTypeClassName()) {
        return cs::Sstring_to_symbol(name);
      }
    }
    cs::ptr fds = cs::snil;
    if (auto* rt = llvm::dyn_cast<clang::RecordType>(tt)) {
      clang::RecordDecl* rd = rt->getDecl();
      llvm::SmallVector<clang::FieldDecl*, 32> fields(rd->fields());
      for (auto itor = fields.end(), itor_begin = fields.begin();
           itor != itor_begin;) {
        --itor;
        clang::FieldDecl* fd = *itor;
        fds = cs::Scons(scheme_list(cs::Sstring_to_symbol(fd->getName().data()),
                          make_ftype<true>(fd->getType())), fds);
      }
      if (rt->isUnionType()) {
        return scheme_list(cs_union_symbol, fds);
      } else if (rt->isStructureType()) {
        return scheme_list(cs_struct_symbol, fds);
      } else {
        llvm::errs() << "unknown record type in make_ftype, only struct and "
                        "union are allowed.";
        type->dump();
        return cs::sfalse;
      }
    }
    assert(false && "unreachabel path");
  }
  if (auto* tt = llvm::dyn_cast<clang::TypedefType>(type)) {
    if (trace_clang) {
      llvm::errs() << "TypedefType\n";
    }
    if (name_ok) {
      if (const char* name = tt->getTypeClassName()) {
        return cs::Sstring_to_symbol(name);
      }
    }
    return make_ftype<name_ok>(tt->desugar());
  }
  if (auto* tt = llvm::dyn_cast<clang::TypeOfExprType>(type)) {
    if (trace_clang) {
      llvm::errs() << "TypeOfExprType\n";
    }
    return make_ftype<true>(tt->desugar());
  }
  llvm::errs() << "unknown type in make_ftype:";
  type->dump();
  return cs::sfalse;
}

template <bool name_ok>
cs::ptr make_ftype(clang::QualType qt) {
  return make_ftype<name_ok>(qt.getTypePtr());
}

class ChezCffiConsumer : public clang::ASTConsumer {
  clang::CompilerInstance& ci;
  clang::ASTContext& context;
  std::unique_ptr<std::ostream> os;

  bool only_original_file;
  bool trace_clang, trace_scheme;

  llvm::DenseSet<const clang::EnumType*> enums_declared;
  llvm::DenseSet<const clang::TypedefDecl*> typedef_declared;
  llvm::DenseSet<const clang::FunctionDecl*> functions_declared;
  llvm::DenseSet<const clang::RecordType*> records_declared;
  llvm::DenseSet<const clang::VarDecl*> vars_declared;

  std::pair<cs::ptr, cs::ptr> emit_enum_field_decls() {
    cs::ptr ret1 = cs::snil, ret2 = cs::snil;
    for (const clang::EnumType* et : enums_declared) {
      clang::EnumDecl* ed = et->getDecl();
      const clang::Type* under_type = ed->getIntegerType().getTypePtr();
      cs::ptr type = make_ftype<true>(under_type);
      clang::DeclarationName name = ed->getDeclName();
      if (!name.isEmpty()) {
        std::string nss = "enum-";
        nss += name.getAsString();
        ret2 = cs::Scons(cs::Scons(cs::Sstring(nss.data()), type), ret2);
      }
      for (clang::EnumConstantDecl* ecd : ed->enumerators()) {
        llvm::StringRef name = ecd->getName();
        uint64_t value = ecd->getInitVal().getLimitedValue();
        std::string nss(name.data(), name.size());
        cs::ptr ns = cs::Sstring_to_symbol(nss.data());
        cs::ptr list = scheme_list(cs_define_constant_symbol, type, ns,
                                   cs::sfixnum(value));
        ret1 = cs::Scons(list, ret1);
      }
    }

    if (trace_scheme) {
      cs::Scall1(cs_display, cs::Sstring("ret1 from emit_enum_field_decls:\n"));
      cs::Scall1(cs_display, ret1);
      cs::Scall0(cs_newline);
      cs::Scall1(cs_display, cs::Sstring("ret2 from emit_enum_field_decls:\n"));
      cs::Scall1(cs_display, ret2);
      cs::Scall0(cs_newline);
    }
    return std::make_pair(ret1, ret2);
  }

  cs::ptr emit_type_definitions() {
    // order all struct/union decls, so that
    // a struct/union's field appears before itself
    // typedef names are ordered
    size_t num = records_declared.size();
    std::vector<const clang::Decl*> ordered;
    std::unordered_set<const clang::Type*> visited;
    struct stack_frame {
      const clang::Decl* decl;
      union {
        bool first;
        clang::RecordDecl::field_iterator itor;
      };
      stack_frame(const clang::Decl* decl_, bool first_): decl(decl_), first(first_) {
      }
      stack_frame(const clang::Decl* decl_, clang::RecordDecl::field_iterator itor_) : decl(decl_), itor(itor_) {
      }
    };
    llvm::SmallVector<stack_frame, 32> stack;

    auto try_insert = [&](const clang::Type* type) {
      if (const auto* et = llvm::dyn_cast<clang::ElaboratedType>(type)) {
        type = et->desugar().getTypePtr();
      }
      if (const auto* rt = llvm::dyn_cast<clang::RecordType>(type)) {
        if (visited.insert(type).second) {
          const auto* rd = rt->getDecl();
          stack.emplace_back(rd, rd->field_begin());
          return true;
        }
      } else if (const auto* tt = llvm::dyn_cast<clang::TypedefType>(type)) {
        if (visited.insert(type).second) {
          stack.emplace_back(tt->getDecl(), true);
        }
      }
      return false;
    };
    auto expand_stack_top = [&]() {
      while (true) {
        const clang::Decl* decl = stack.back().decl;
        if (const auto* rd = llvm::dyn_cast<clang::RecordDecl>(decl)) {
          auto itor = stack.back().itor;
          while (itor != rd->field_end()) {
            clang::FieldDecl* fd = *(itor++);
            if (try_insert(fd->getType().getTypePtr())) {
              (stack.end() - 2)->itor = itor;
              goto again;
            }
          }
        } else {
          const auto* td = llvm::cast<clang::TypedefDecl>(decl);
          const clang::Type* type = td->getTypeSourceInfo()->getType().getTypePtr();
          if (stack.back().first) {
            stack.back().first = false;
            if (try_insert(type)) {
              goto again;
            }
          }
        }
        ordered.push_back(decl);
        stack.pop_back();
        return;
      again:
        continue;
      }
    };
    for (const clang::RecordType* rt : records_declared) {
      const clang::Type* type = static_cast<const clang::Type*>(rt);
      if (visited.insert(type).second) {
        const clang::RecordDecl* rd = rt->getDecl();
        stack.emplace_back(rd, rd->field_begin());
        while (!stack.empty()) {
          expand_stack_top();
        }
      }
    }
    for (const clang::TypedefDecl* td: typedef_declared) {
      const clang::Type* type1 = td->getTypeForDecl();
      const clang::Type* type = td->getTypeSourceInfo()->getType().getTypePtr();
      if (visited.insert(type).second) {
        stack.emplace_back(td, true);
        while (!stack.empty()) {
          expand_stack_top();
        }
      }
    }
    cs::ptr ret = cs::snil;
    for (const clang::Decl* decl: ordered) {
      if (const auto* rd = llvm::dyn_cast<clang::RecordDecl>(decl)) {
          //llvm::errs() << "record type in emit_type_definitions:\n";
        std::string name;
        if (rd->isStruct()) {
          name = "struct-";
        } else if (rd->isUnion()) {
          name = "union-";
        } else {
          llvm::errs() << "unknown non-struct/union record in emit_type_definitions:\n";
          decl->dump();
          continue;
        }
        name += rd->getNameAsString();
        ret = cs::Scons(cs::Scons(cs::Sstring(name.data()), make_ftype<false>(rd->getTypeForDecl())), ret);
      } else if (const auto* td = llvm::dyn_cast<clang::TypedefDecl>(decl)) {
          //llvm::errs() << "typedef type in emit_type_definitions:\n";
          std::string name = td->getNameAsString();
          const clang::Type* type = td->getTypeSourceInfo()->getType().getTypePtr();
          ret = cs::Scons(cs::Scons(cs::Sstring(name.data()), make_ftype<false>(type)), ret);
          //ret = cs::Scons(cs::Sstring(name.data()), ret);
      } else {
          llvm::errs() << "unknown type in emit_type_definitions:\n";
          decl->dump();
          continue;
      }
      /*
      clang::RecordDecl* rd = rt->getDecl();
      clang::DeclarationName name = rd->getDeclName();
      if (name.isEmpty()) {
        continue;
      }
      std::string nss;
      nss += name.getAsString();
      cs::ptr p = make_ftype<false>(rt);
      ret = cs::Scons(cs::Scons(cs::Sstring(nss.data()), p), ret);
      */
    }
    if (trace_scheme) {
      cs::Scall1(cs_display, cs::Sstring("ret from emit_type_definitions:\n"));
      cs::Scall1(cs_display, ret);
      cs::Scall0(cs_newline);
    }
    return ret;
  }

  cs::ptr emit_function_decls() {
    cs::ptr ret = cs::snil;
    for (const clang::FunctionDecl* fd: functions_declared) {
      llvm::StringRef name = fd->getName();
      std::string nss(name.data(), name.size());
      ret = cs::Scons(cs::Scons(cs::Sstring_to_symbol(nss.data()), make_ftype<false>(fd->getType())), ret);
    }
    if (trace_scheme) {
      cs::Scall1(cs_display, cs::Sstring("ret from emit_function_decls:\n"));
      cs::Scall1(cs_display, ret);
      cs::Scall0(cs_newline);
    }
    return ret;
  }

  cs::ptr emit_var_decls() {
    cs::ptr ret = cs::snil;
    for (const clang::VarDecl* vd: vars_declared) {
      llvm::StringRef name = vd->getName();
      std::string nss(name.data(), name.size());
      ret = cs::Scons(cs::Scons(cs::Sstring_to_symbol(nss.data()), make_ftype<true>(vd->getType())), ret);
    }
    if (trace_scheme) {
      cs::Scall1(cs_display, cs::Sstring("ret from emit_var_decls:\n"));
      cs::Scall1(cs_display, ret);
      cs::Scall0(cs_newline);
    }
    return ret;
  }

  cs::ptr emit_scheme_code() {
    if (trace_clang) {
      llvm::errs() << "emit_scheme_code:\n";
      llvm::errs() << "enums: \n";
      for (const auto* et : enums_declared) {
        et->dump();
      }
      llvm::errs() << "typedefs: \n";
      for (const clang::TypedefDecl* td : typedef_declared) {
        td->dump();
      }
      llvm::errs() << "functions: \n";
      for (const auto* fd : functions_declared) {
        fd->dump();
      }
      llvm::errs() << "records: \n";
      for (const auto* rd : records_declared) {
        rd->dump();
      }
      llvm::errs() << "vars: \n";
      for (const auto* vd : vars_declared) {
        vd->dump();
      }
    }
    auto ep = emit_enum_field_decls();
    cs::ptr p1 = ep.first;
    cs::ptr p2 = emit_type_definitions();
    cs::ptr p3 = emit_function_decls();
    cs::ptr p4 = emit_var_decls();
    //return scheme_list(p1, p2, p3, p4);
    return cs::snil;
  }

  bool should_skip(clang::Decl* D) {
    return only_original_file &&
           !context.getSourceManager().isInMainFile(D->getLocation());
  }
  bool handle_top_level_decl(clang::Decl* D) {
    if (trace_clang) {
      llvm::errs() << "handle_top_level_decl\n";
      D->dump();
    }
    if (auto* td = llvm::dyn_cast<clang::TypedefDecl>(D)) {
      typedef_declared.insert(td);
      return true;
    }
    if (auto* rd = llvm::dyn_cast<clang::RecordDecl>(D)) {
      if (!rd->isCompleteDefinition()) {
        return true;
      }
      records_declared.insert(
          llvm::cast<clang::RecordType>(rd->getTypeForDecl()));
      return true;
    }
    if (auto* ed = llvm::dyn_cast<clang::EnumDecl>(D)) {
      if (!ed->isComplete()) {
        return true;
      }
      enums_declared.insert(llvm::cast<clang::EnumType>(ed->getTypeForDecl()));
      return true;
    }
    if (auto* fd = llvm::dyn_cast<clang::FunctionDecl>(D)) {
      functions_declared.insert(fd);
      return true;
    }
    if (auto* vd = llvm::dyn_cast<clang::VarDecl>(D)) {
      vars_declared.insert(vd);
      return true;
    }
    llvm::errs() << "met unknown decl: \n";
    D->dump();
    return false;
  }

 public:
  ChezCffiConsumer(clang::CompilerInstance& ci_,
                   std::vector<std::string> boot_files,
                   std::unique_ptr<std::ostream> os_, bool oof_, bool trace_clang_, bool trace_scheme_)
      : ci(ci_), context(ci.getASTContext()),
        os(std::move(os_)),
        only_original_file(oof_),
        trace_clang(trace_clang_), trace_scheme(trace_scheme_) {
          TryInit(boot_files);
        }
  void HandleTranslationUnit(clang::ASTContext& Ctx) override {
    if (trace_clang) {
      llvm::errs() << "HandleTranslationUnit\n";
    }
    emit_scheme_code();
  }
  bool HandleTopLevelDecl(clang::DeclGroupRef D) override {
    if (should_skip(*D.begin())) {
      return true;
    }
    if (trace_clang) {
      llvm::errs() << "HandleTopLevelDecl\n";
      for (auto* decl : llvm::make_range(D.begin(), D.end())) {
        decl->dump();
      }
    }
    for (clang::Decl* decl : llvm::make_range(D.begin(), D.end())) {
      if (!handle_top_level_decl(decl)) {
        return false;
      }
    }
    return true;
  }

  bool shouldSkipFunctionBody(clang::Decl*) override { return true; }
};

class ChezCffiAction : public clang::PluginASTAction {
  std::string output_file_name;
  bool only_original_file, trace_clang, trace_scheme;
  std::vector<std::string> boot_files;

 public:
  ChezCffiAction() : only_original_file(false), trace_clang(false), trace_scheme(false) {}
  std::unique_ptr<clang::ASTConsumer> CreateASTConsumer(
      clang::CompilerInstance& CI, llvm::StringRef) override {
    llvm::errs() << "chez scheme vesion: " << cs::Skernel_version() << '\n';
    auto os = std::make_unique<std::ofstream>(output_file_name);
    return std::make_unique<ChezCffiConsumer>(CI, boot_files, std::move(os),
                                              only_original_file, trace_clang, trace_scheme);
  }

  bool ParseArgs(const clang::CompilerInstance&,
                 const std::vector<std::string>& args) override {
    bool has_output_file = false;
    for (int i = 0, ie = args.size(); i < ie; ++i) {
      if (args[i] == "-only-main") {
        only_original_file = true;
        continue;
      }
      if (args[i] == "-trace-clang") {
        trace_clang = true;
        continue;
      }
      if (args[i] == "-trace-scheme") {
        trace_scheme = true;
        continue;
      }
      if (args[i] == "-o") {
        if (has_output_file) {
          llvm::errs() << "can only specify one output file\n";
          return false;
        }
        has_output_file = true;
        ++i;
        if (i == ie) {
          llvm::errs() << "-o option must be followed by file name\n";
          return false;
        }
        output_file_name = args[i];
        continue;
      }
      if (args[i] == "-b") {
        ++i;
        if (i == ie) {
          llvm::errs() << "-b option must be followed by boot file name\n";
          return false;
        }
        boot_files.push_back(args[i]);
        continue;
      }
      llvm::errs() << "unknown argument to chez_cffi: " << args[i] << '\n';
      return false;
    }
    if (!has_output_file) {
      llvm::errs() << "please specify output file with -o\n";
      return false;
    }
    if (!boot_files.size()) {
      llvm::errs() << "please specify boot file with -b\n";
      return false;
    }
    return true;
  }
  bool usesPreprocessorOnly() const override { return false; }
};
clang::FrontendPluginRegistry::Add<ChezCffiAction> X(
    "chez-cffi", "generate chez scheme ffi for c headers");
}  // namespace

