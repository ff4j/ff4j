
// button
!function ($) {

  "use strict"; // jshint ;_;

  var dismiss = '[data-dismiss="alert"]'
    , Alert = function (el) {
        $(el).on('click', dismiss, this.close)
      }

  Alert.prototype.close = function (e) {
    var $this = $(this)
      , selector = $this.attr('data-target')
      , $parent

    if (!selector) {
      selector = $this.attr('href')
      selector = selector && selector.replace(/.*(?=#[^\s]*$)/, '') //strip for ie7
    }

    $parent = $(selector)

    e && e.preventDefault()

    $parent.length || ($parent = $this.hasClass('alert') ? $this : $this.parent())

    $parent.trigger(e = $.Event('close'))

    if (e.isDefaultPrevented()) return

    $parent.removeClass('in')

    function removeElement() {
      $parent
        .trigger('closed')
        .remove()
    }

    $.support.transition && $parent.hasClass('fade') ?
      $parent.on($.support.transition.end, removeElement) :
      removeElement()
  }

  var old = $.fn.alert

  $.fn.alert = function (option) {
    return this.each(function () {
      var $this = $(this)
        , data = $this.data('alert')
      if (!data) $this.data('alert', (data = new Alert(this)))
      if (typeof option == 'string') data[option].call($this)
    })
  }

  $.fn.alert.Constructor = Alert

  $.fn.alert.noConflict = function () {
    $.fn.alert = old
    return this
  }

  $(document).on('click.alert.data-api', dismiss, Alert.prototype.close)

}(window.jQuery);


// button
!function ($) {
  "use strict"; // jshint ;_;


 /* BUTTON PUBLIC CLASS DEFINITION
  * ============================== */

  var Button = function (element, options) {
    this.$element = $(element)
    this.options = $.extend({}, $.fn.button.defaults, options)
  }

  Button.prototype.setState = function (state) {
    var d = 'disabled'
      , $el = this.$element
      , data = $el.data()
      , val = $el.is('input') ? 'val' : 'html'

    state = state + 'Text'
    data.resetText || $el.data('resetText', $el[val]())

    $el[val](data[state] || this.options[state])

    // push to event loop to allow forms to submit
    setTimeout(function () {
      state == 'loadingText' ?
        $el.addClass(d).attr(d, d) :
        $el.removeClass(d).removeAttr(d)
    }, 0)
  }

  Button.prototype.toggle = function () {
    var $parent = this.$element.closest('[data-toggle="buttons-radio"]')

    $parent && $parent
      .find('.active')
      .removeClass('active')

    this.$element.toggleClass('active')
  }


 /* BUTTON PLUGIN DEFINITION
  * ======================== */

  var old = $.fn.button

  $.fn.button = function (option) {
    return this.each(function () {
      var $this = $(this)
        , data = $this.data('button')
        , options = typeof option == 'object' && option
      if (!data) $this.data('button', (data = new Button(this, options)))
      if (option == 'toggle') data.toggle()
      else if (option) data.setState(option)
    })
  }

  $.fn.button.defaults = {
    loadingText: 'loading...'
  }

  $.fn.button.Constructor = Button


 /* BUTTON NO CONFLICT
  * ================== */

  $.fn.button.noConflict = function () {
    $.fn.button = old
    return this
  }


 /* BUTTON DATA-API
  * =============== */

  $(document).on('click.button.data-api', '[data-toggle^=button]', function (e) {
    var $btn = $(e.target)
    if (!$btn.hasClass('btn')) $btn = $btn.closest('.btn')
    $btn.button('toggle')
  })

}(window.jQuery);

// dropdown
!function ($) {

	  "use strict"; // jshint ;_;


	 /* DROPDOWN CLASS DEFINITION
	  * ========================= */

	  var toggle = '[data-toggle=dropdown]'
	    , Dropdown = function (element) {
	        var $el = $(element).on('click.dropdown.data-api', this.toggle)
	        $('html').on('click.dropdown.data-api', function () {
	          $el.parent().removeClass('open')
	        })
	      }

	  Dropdown.prototype = {

	    constructor: Dropdown

	  , toggle: function (e) {
	      var $this = $(this)
	        , $parent
	        , isActive

	      if ($this.is('.disabled, :disabled')) return

	      $parent = getParent($this)

	      isActive = $parent.hasClass('open')

	      clearMenus()

	      if (!isActive) {
	        $parent.toggleClass('open')
	      }

	      $this.focus()

	      return false
	    }

	  , keydown: function (e) {
	      var $this
	        , $items
	        , $active
	        , $parent
	        , isActive
	        , index

	      if (!/(38|40|27)/.test(e.keyCode)) return

	      $this = $(this)

	      e.preventDefault()
	      e.stopPropagation()

	      if ($this.is('.disabled, :disabled')) return

	      $parent = getParent($this)

	      isActive = $parent.hasClass('open')

	      if (!isActive || (isActive && e.keyCode == 27)) {
	        if (e.which == 27) $parent.find(toggle).focus()
	        return $this.click()
	      }

	      $items = $('[role=menu] li:not(.divider):visible a', $parent)

	      if (!$items.length) return

	      index = $items.index($items.filter(':focus'))

	      if (e.keyCode == 38 && index > 0) index--                                        // up
	      if (e.keyCode == 40 && index < $items.length - 1) index++                        // down
	      if (!~index) index = 0

	      $items
	        .eq(index)
	        .focus()
	    }

	  }

	  function clearMenus() {
	    $(toggle).each(function () {
	      getParent($(this)).removeClass('open')
	    })
	  }

	  function getParent($this) {
	    var selector = $this.attr('data-target')
	      , $parent

	    if (!selector) {
	      selector = $this.attr('href')
	      selector = selector && /#/.test(selector) && selector.replace(/.*(?=#[^\s]*$)/, '') //strip for ie7
	    }

	    $parent = selector && $(selector)

	    if (!$parent || !$parent.length) $parent = $this.parent()

	    return $parent
	  }


	  /* DROPDOWN PLUGIN DEFINITION
	   * ========================== */

	  var old = $.fn.dropdown

	  $.fn.dropdown = function (option) {
	    return this.each(function () {
	      var $this = $(this)
	        , data = $this.data('dropdown')
	      if (!data) $this.data('dropdown', (data = new Dropdown(this)))
	      if (typeof option == 'string') data[option].call($this)
	    })
	  }

	  $.fn.dropdown.Constructor = Dropdown


	 /* DROPDOWN NO CONFLICT
	  * ==================== */

	  $.fn.dropdown.noConflict = function () {
	    $.fn.dropdown = old
	    return this
	  }


	  /* APPLY TO STANDARD DROPDOWN ELEMENTS
	   * =================================== */

	  $(document)
	    .on('click.dropdown.data-api', clearMenus)
	    .on('click.dropdown.data-api', '.dropdown form', function (e) { e.stopPropagation() })
	    .on('click.dropdown-menu', function (e) { e.stopPropagation() })
	    .on('click.dropdown.data-api'  , toggle, Dropdown.prototype.toggle)
	    .on('keydown.dropdown.data-api', toggle + ', [role=menu]' , Dropdown.prototype.keydown)

	}(window.jQuery);

	// modal

	!function ($) {

	  "use strict"; // jshint ;_;


	 /* MODAL CLASS DEFINITION
	  * ====================== */

	  var Modal = function (element, options) {
	    this.options = options
	    this.$element = $(element)
	      .delegate('[data-dismiss="modal"]', 'click.dismiss.modal', $.proxy(this.hide, this))
	    this.options.remote && this.$element.find('.modal-body').load(this.options.remote)
	  }

	  Modal.prototype = {

	      constructor: Modal

	    , toggle: function () {
	        return this[!this.isShown ? 'show' : 'hide']()
	      }

	    , show: function () {
	        var that = this
	          , e = $.Event('show')

	        this.$element.trigger(e)

	        if (this.isShown || e.isDefaultPrevented()) return

	        this.isShown = true

	        this.escape()

	        this.backdrop(function () {
	          var transition = $.support.transition && that.$element.hasClass('fade')

	          if (!that.$element.parent().length) {
	            that.$element.appendTo(document.body) //don't move modals dom position
	          }

	          that.$element.show()

	          if (transition) {
	            that.$element[0].offsetWidth // force reflow
	          }

	          that.$element
	            .addClass('in')
	            .attr('aria-hidden', false)

	          that.enforceFocus()

	          transition ?
	            that.$element.one($.support.transition.end, function () { that.$element.focus().trigger('shown') }) :
	            that.$element.focus().trigger('shown')

	        })
	      }

	    , hide: function (e) {
	        e && e.preventDefault()

	        var that = this

	        e = $.Event('hide')

	        this.$element.trigger(e)

	        if (!this.isShown || e.isDefaultPrevented()) return

	        this.isShown = false

	        this.escape()

	        $(document).off('focusin.modal')

	        this.$element
	          .removeClass('in')
	          .attr('aria-hidden', true)

	        $.support.transition && this.$element.hasClass('fade') ?
	          this.hideWithTransition() :
	          this.hideModal()
	      }

	    , enforceFocus: function () {
	        var that = this
	        $(document).on('focusin.modal', function (e) {
	          if (that.$element[0] !== e.target && !that.$element.has(e.target).length) {
	            that.$element.focus()
	          }
	        })
	      }

	    , escape: function () {
	        var that = this
	        if (this.isShown && this.options.keyboard) {
	          this.$element.on('keyup.dismiss.modal', function ( e ) {
	            e.which == 27 && that.hide()
	          })
	        } else if (!this.isShown) {
	          this.$element.off('keyup.dismiss.modal')
	        }
	      }

	    , hideWithTransition: function () {
	        var that = this
	          , timeout = setTimeout(function () {
	              that.$element.off($.support.transition.end)
	              that.hideModal()
	            }, 500)

	        this.$element.one($.support.transition.end, function () {
	          clearTimeout(timeout)
	          that.hideModal()
	        })
	      }

	    , hideModal: function () {
	        var that = this
	        this.$element.hide()
	        this.backdrop(function () {
	          that.removeBackdrop()
	          that.$element.trigger('hidden')
	        })
	      }

	    , removeBackdrop: function () {
	        this.$backdrop && this.$backdrop.remove()
	        this.$backdrop = null
	      }

	    , backdrop: function (callback) {
	        var that = this
	          , animate = this.$element.hasClass('fade') ? 'fade' : ''

	        if (this.isShown && this.options.backdrop) {
	          var doAnimate = $.support.transition && animate

	          this.$backdrop = $('<div class="modal-backdrop ' + animate + '" />')
	            .appendTo(document.body)

	          this.$backdrop.click(
	            this.options.backdrop == 'static' ?
	              $.proxy(this.$element[0].focus, this.$element[0])
	            : $.proxy(this.hide, this)
	          )

	          if (doAnimate) this.$backdrop[0].offsetWidth // force reflow

	          this.$backdrop.addClass('in')

	          if (!callback) return

	          doAnimate ?
	            this.$backdrop.one($.support.transition.end, callback) :
	            callback()

	        } else if (!this.isShown && this.$backdrop) {
	          this.$backdrop.removeClass('in')

	          $.support.transition && this.$element.hasClass('fade')?
	            this.$backdrop.one($.support.transition.end, callback) :
	            callback()

	        } else if (callback) {
	          callback()
	        }
	      }
	  }


	 /* MODAL PLUGIN DEFINITION
	  * ======================= */

	  var old = $.fn.modal

	  $.fn.modal = function (option) {
	    return this.each(function () {
	      var $this = $(this)
	        , data = $this.data('modal')
	        , options = $.extend({}, $.fn.modal.defaults, $this.data(), typeof option == 'object' && option)
	      if (!data) $this.data('modal', (data = new Modal(this, options)))
	      if (typeof option == 'string') data[option]()
	      else if (options.show) data.show()
	    })
	  }

	  $.fn.modal.defaults = {
	      backdrop: true
	    , keyboard: true
	    , show: true
	  }

	  $.fn.modal.Constructor = Modal


	 /* MODAL NO CONFLICT
	  * ================= */

	  $.fn.modal.noConflict = function () {
	    $.fn.modal = old
	    return this
	  }


	 /* MODAL DATA-API
	  * ============== */

	  $(document).on('click.modal.data-api', '[data-toggle="modal"]', function (e) {
	    var $this = $(this)
	      , href = $this.attr('href')
	      , $target = $($this.attr('data-target') || (href && href.replace(/.*(?=#[^\s]+$)/, ''))) //strip for ie7
	      , option = $target.data('modal') ? 'toggle' : $.extend({ remote:!/#/.test(href) && href }, $target.data(), $this.data())

	    e.preventDefault()

	    $target
	      .modal(option)
	      .one('hide', function () {
	        $this.focus()
	      })
	  })

	}(window.jQuery);
	
	// transition

	!function ($) {

	  "use strict"; // jshint ;_;


	  /* CSS TRANSITION SUPPORT (http://www.modernizr.com/)
	   * ======================================================= */

	  $(function () {

	    $.support.transition = (function () {

	      var transitionEnd = (function () {

	        var el = document.createElement('bootstrap')
	          , transEndEventNames = {
	               'WebkitTransition' : 'webkitTransitionEnd'
	            ,  'MozTransition'    : 'transitionend'
	            ,  'OTransition'      : 'oTransitionEnd otransitionend'
	            ,  'transition'       : 'transitionend'
	            }
	          , name

	        for (name in transEndEventNames){
	          if (el.style[name] !== undefined) {
	            return transEndEventNames[name]
	          }
	        }

	      }())

	      return transitionEnd && {
	        end: transitionEnd
	      }

	    })()

	  })

	}(window.jQuery);



