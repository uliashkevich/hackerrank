package interview.domdistance

import interview.domdistance.Solution._
import org.junit.runner.RunWith
import org.scalatest.FunSuite
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class SolutionTest extends FunSuite {
  test("parsePath") {
    assert(Tag.parsePath("h1 div#id div#inner.frame.back img.photo") ==
      IndexedSeq(
        Tag("h1"),
        Tag("div", Some("id")),
        Tag("div", Some("inner"), Set("frame", "back")),
        Tag("img", None, Set("photo"))))
  }

  test("div -> div") {
    assert(distance(IndexedSeq(Tag("div")), IndexedSeq(Tag("div"))) == 0)
  }

  test("div -> a") {
    assert(distance(IndexedSeq(Tag("div")), IndexedSeq(Tag("a"))) == 1)
  }

  test("div -> div#id") {
    assert(distance(IndexedSeq(Tag("div")), IndexedSeq(Tag("div", Some("id")))) == 1)
  }

  test("div#id -> div") {
    assert(distance(IndexedSeq(Tag("div", Some("id"))), IndexedSeq(Tag("div"))) == 1)
  }

  test("div#id.btn -> div") {
    assert(distance(Tag.parsePath("div#id.btn"), Tag.parsePath("div")) == 2)
  }

  test("div#id span.text a#link.btn -> a#id.btn") {
    assert(distance(Tag.parsePath("div#id span.text a#link.btn"), Tag.parsePath("a#id.btn")) == 4)
  }

  test("a#id.btn -> div#id span.text a#link.btn") {
    assert(distance(Tag.parsePath("a#id.btn"), Tag.parsePath("div#id span.text a#link.btn")) == 6)
  }

  test("large 1") {
    assert(distance(
      Tag.parsePath("div#shell div#page.active.tabContent div#main div.baseLayout.wrap div.column.last div.layout.spanAB div.abColumn.column div.layout.module.wideB div.aColumn.column.opening div.columnGroup div.story p.summary"),
      Tag.parsePath("div#page.active.tabContent div#main div.baseLayout.wrap div.column.last div.layout.spanAB div.abColumn.column div.layout.module.wideB div.aColumn.column.opening div.columnGroup.first div.story p.summary")
    ) == 2)
  }

  test("large 2") {
    assert(distance(
      Tag.parsePath("div#masthead.billboard-layout.cf.main-col div#yui_3_8_1_1_1382751082490_1862.main-row-wrapper div#default-p_13838465.mod.view_default div#default-p_13838465-bd.bd.type_masthead.type_masthead_default div#yui_3_8_1_1_1382751082490_1861.clearfix.lightbg.mh-wrap.us.y-fp-pg-grad form#p_13838465-searchform.search-form fieldset#yui_3_8_1_1_1382751082490_1860.compact-enabled-fieldset div#p_13838465-searchwrapper.searchwrapper.selected.tabpanel div#yui_3_8_1_1_1382751082490_1859.focus.searchwrapper-border.y-srch-brdr div#fp-search-bdr.brdr-focus.clearfix.searchwrapper-inner.y-glbl-srch-bg-img div#yui_3_8_1_1_1382751082490_1858.input-wrapper input#p_13838465-p.compact-input-enabled.input-long.input-query.med-large"),
      Tag.parsePath("div#masthead.billboard-layout.cf.main-col div#yui_3_8_1_1_1382751082490_1862.main-row-wrapper div#default-p_13838465.mod.view_default div#default-p_13838465-bd.bd.type_masthead.type_masthead_default div#yui_3_8_1_1_1382751082490_1861.clearfix.lightbg.mh-wrap.us.y-fp-pg-grad form#p_13838465-searchform.search-form fieldset#yui_3_8_1_1_1382751082490_1860.compact-enabled-fieldset div#p_13838465-searchwrapper.searchwrapper.selected.tabpanel div#yui_3_8_1_1_1382751082490_1859.searchwrapper-border.y-srch-brdr div#fp-search-bdr.clearfix.searchwrapper-inner.y-glbl-srch-bg-img div#yui_3_8_1_1_1382751082490_1858.input-wrapper input#p_13838465-p.compact-input-enabled.input-long.input-query.med-large")
    ) == 2)
  }

  test("large 3") {
    assert(distance(
      Tag.parsePath("header.mh div.mh-stripe div.mh-stripe-wrap ul.mh-user-menu li.last a.omniture-tagged.omniture-tagged-291.show-login"),
      Tag.parsePath("div.ec-overlay div.login-wrap form#user-login.clearfix.context-user_login.ec-social.user-form div div#edit-name-wrapper.clearfix.form-item input#edit-name.form-email.form-text.required")
    ) == 24)
  }

  test("large 4") {
    assert(distance(
      Tag.parsePath("header.cf.header div.nav-bar div.lc form.search-form fieldset input.search-field"),
      Tag.parsePath("div.fluid.flush.homepage.split div.flush.lc.lc-island div.l-two-col div.l-main-container div.l-main ul#river1.lc-padding.river li#905418.river-block div.block.block-thumb div.block-content p.excerpt")
    ) == 30)
  }
}
